# Тесты защиты входа от подбора пароля (R/login_protection.R, modules/mod_auth.R,
# кнопка снятия блокировки в modules/mod_tab_access.R).
# Запуск из корня проекта: Rscript tests/test_login_protection.R

suppressMessages({library(shiny); library(DBI); library(RSQLite); library(dplyr); library(tibble); library(DT)})
source("tests/helper.R")
for (f in c("R/passwords.R", "R/actions.R", "R/logging.R", "R/access.R", "R/login_protection.R", "R/user_blocking.R")) source(f)
source("modules/mod_auth.R"); source("modules/mod_tab_access.R")

# функции, результат которых в тестах не нужен, не должны печатать значения
dbExecute <- function(...) invisible(DBI::dbExecute(...))
create_user <- local({ f <- create_user; function(...) invisible(f(...)) })
clear_login_attempts <- local({ f <- clear_login_attempts; function(...) invisible(f(...)) })

add_attempt <- function(con, login, success, minutes_ago = 0, seconds_ago = NULL) {
  offset <- if (!is.null(seconds_ago)) sprintf("-%d seconds", as.integer(seconds_ago)) else sprintf("-%d minutes", as.integer(minutes_ago))
  dbExecute(con, "INSERT INTO login_attempts (login, success, attempted_at) VALUES (?, ?, datetime('now', ?))", params = list(login, as.integer(success), offset))
}
count_rows <- function(con, table) dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s", table))$n

# ======================= 1. Схема: новая база и «база как на проде» =======================
db_new <- temp_path("t_new.db"); unlink(db_new)
conf <<- list(database_settings = list(app_data_base = db_new), logging = list(action_log = TRUE))
invisible(capture.output(suppressWarnings(source("create_db.R"))))
con <- dbConnect(SQLite(), db_new)
chk("create_db.R: таблица login_attempts создана в новой базе", "login_attempts" %in% dbListTables(con))
dbDisconnect(con)

# старая база без таблицы login_attempts, с данными (как работающий prod до обновления)
db_old <- temp_path("t_old.db"); unlink(db_old)
old <- dbConnect(SQLite(), db_old)
dbExecute(old, "CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, login TEXT UNIQUE NOT NULL, password TEXT NOT NULL, role TEXT NOT NULL)")
dbExecute(old, "CREATE TABLE auth_sessions (id INTEGER PRIMARY KEY AUTOINCREMENT, user_login TEXT NOT NULL, token_hash TEXT NOT NULL, created_at DATETIME DEFAULT CURRENT_TIMESTAMP, expires_at DATETIME NOT NULL, revoked INTEGER DEFAULT 0)")
dbExecute(old, "CREATE TABLE action_log (id INTEGER PRIMARY KEY AUTOINCREMENT, datetime DATETIME, func_name TEXT, user TEXT, session_id TEXT, value TEXT)")
create_user(old, "alsey", "secret1", "admin"); create_user(old, "ida", "secret2", "viewer")
users_before <- dbGetQuery(old, "SELECT * FROM users ORDER BY id")
chk("старая база: таблицы login_attempts ещё нет", !"login_attempts" %in% dbListTables(old))
chk("prepare_login_protection на старой базе завершился без ошибок", isTRUE(suppressMessages(prepare_login_protection(old))))
chk("старая база: таблица login_attempts создана", "login_attempts" %in% dbListTables(old))
chk("старая база: пользователи и пароли не изменились", identical(users_before, dbGetQuery(old, "SELECT * FROM users ORDER BY id")))
chk("повторный запуск prepare_login_protection безопасен", isTRUE(suppressMessages(prepare_login_protection(old))) && count_rows(old, "users") == 2)
dbDisconnect(old)

# ======================= 2. Логика блокировки =======================
db <- temp_path("t_lock.db"); unlink(db)
con <- dbConnect(SQLite(), db)
dbExecute(con, "CREATE TABLE auth_sessions (id INTEGER PRIMARY KEY AUTOINCREMENT, user_login TEXT NOT NULL, token_hash TEXT NOT NULL, created_at DATETIME DEFAULT CURRENT_TIMESTAMP, expires_at DATETIME NOT NULL, revoked INTEGER DEFAULT 0)")
ensure_login_attempts_table(con)
conf <<- list()   # настройки по умолчанию: 5 попыток, 15 минут, 30 в минуту

s <- login_security_settings()
chk("настройки по умолчанию: 5 попыток, 15 минут, 30 в минуту", s$max_failed == 5 && s$lockout_minutes == 15 && s$max_per_minute == 30)
s2 <- login_security_settings(list(security = list(max_failed_logins = 3, lockout_minutes = 10, max_failed_logins_per_minute = 7)))
chk("настройки из config.yaml применяются", s2$max_failed == 3 && s2$lockout_minutes == 10 && s2$max_per_minute == 7)
s3 <- login_security_settings(list(security = list(max_failed_logins = "abc", lockout_minutes = -5)))
chk("некорректные настройки заменяются значениями по умолчанию", s3$max_failed == 5 && s3$lockout_minutes == 15)

for (i in 1:4) record_login_attempt(con, "alsey", FALSE)
chk("4 неудачи: вход ещё не заблокирован", login_lock_remaining(con, "alsey") == 0)
record_login_attempt(con, "alsey", FALSE)
rem <- login_lock_remaining(con, "alsey")
chk("5 неудач: вход заблокирован примерно на 15 минут", rem > 14 * 60 && rem <= 15 * 60)
chk("другой логин не заблокирован", login_lock_remaining(con, "ida") == 0)
chk("регистр и пробелы в логине не обходят блокировку", login_lock_remaining(con, "  ALSEY ") > 0 && login_lock_remaining(con, "Alsey") > 0)

invisible(clear_login_attempts(con, "alsey"))
chk("снятие блокировки работает", login_lock_remaining(con, "alsey") == 0)

for (i in 1:3) record_login_attempt(con, "ida", FALSE)
record_login_attempt(con, "ida", TRUE)
for (i in 1:4) record_login_attempt(con, "ida", FALSE)
chk("успешный вход обнуляет счётчик (3 неудачи, успех, 4 неудачи -> без блокировки)", login_lock_remaining(con, "ida") == 0)
record_login_attempt(con, "ida", FALSE)
chk("после успеха 5 новых неудач блокируют", login_lock_remaining(con, "ida") > 0)

dbExecute(con, "DELETE FROM login_attempts")
for (i in 1:5) add_attempt(con, "old1", FALSE, minutes_ago = 20)
chk("5 неудач 20 минут назад не считаются (вне окна)", login_lock_remaining(con, "old1") == 0)
for (i in 1:5) add_attempt(con, "old2", FALSE, minutes_ago = 16)
chk("блокировка истекла через 16 минут", login_lock_remaining(con, "old2") == 0)
for (i in 1:4) add_attempt(con, "mid", FALSE, minutes_ago = 14); add_attempt(con, "mid", FALSE, minutes_ago = 10)
rem <- login_lock_remaining(con, "mid")
chk("блокировка считается от последней неудачи (осталось около 5 минут)", rem > 4 * 60 && rem <= 5 * 60 + 5)
chk("несуществующий логин блокируется так же", { for (i in 1:5) record_login_attempt(con, "no_such_user", FALSE); login_lock_remaining(con, "no_such_user") > 0 })
chk("пустой логин не записывается и не блокируется", { n0 <- count_rows(con, "login_attempts"); record_login_attempt(con, "   ", FALSE); record_login_attempt(con, NA, FALSE); count_rows(con, "login_attempts") == n0 && login_lock_remaining(con, "") == 0 })
chk("очень длинный логин усекается до 64 символов", { record_login_attempt(con, strrep("a", 500), FALSE); max(nchar(dbGetQuery(con, "SELECT login FROM login_attempts")$login)) <= 64 })
chk("настройка max_failed = 3 применяется", { dbExecute(con, "DELETE FROM login_attempts"); for (i in 1:3) record_login_attempt(con, "x3", FALSE); login_lock_remaining(con, "x3", max_failed = 3) > 0 && login_lock_remaining(con, "x3", max_failed = 5) == 0 })

# --- общий лимит по всем логинам
dbExecute(con, "DELETE FROM login_attempts")
for (i in 1:29) add_attempt(con, paste0("spray", i), FALSE, seconds_ago = 5)
chk("29 неудач за минуту по разным логинам: общий лимит не сработал", !login_global_throttled(con))
add_attempt(con, "spray30", FALSE, seconds_ago = 5)
chk("30 неудач за минуту по разным логинам: общий лимит сработал", login_global_throttled(con))
dbExecute(con, "DELETE FROM login_attempts")
for (i in 1:40) add_attempt(con, paste0("spray", i), FALSE, seconds_ago = 120)
chk("неудачи старше минуты в общий лимит не входят", !login_global_throttled(con))

# --- уборка
dbExecute(con, "DELETE FROM login_attempts")
add_attempt(con, "a", FALSE, minutes_ago = 60 * 25); add_attempt(con, "a", FALSE, minutes_ago = 60)
dbExecute(con, "INSERT INTO auth_sessions (user_login, token_hash, created_at, expires_at, revoked) VALUES
  ('u', 'expired_old',   datetime('now','-90 days'), datetime('now','-60 days'), 0),
  ('u', 'revoked_old',   datetime('now','-50 days'), datetime('now','+10 days'), 1),
  ('u', 'revoked_new',   datetime('now','-2 days'),  datetime('now','+28 days'), 1),
  ('u', 'expired_new',   datetime('now','-35 days'), datetime('now','-5 days'),  0),
  ('u', 'active',        datetime('now','-1 days'),  datetime('now','+29 days'), 0)")
res <- cleanup_security_tables(con)
left <- dbGetQuery(con, "SELECT token_hash FROM auth_sessions")$token_hash
chk("уборка: попытки старше суток удалены, свежие остались", res$login_attempts == 1 && count_rows(con, "login_attempts") == 1)
chk("уборка: удалены давно просроченные и давно отозванные токены", !any(c("expired_old", "revoked_old") %in% left))
chk("уборка: активные, недавно отозванные и недавно просроченные токены остались", all(c("active", "revoked_new", "expired_new") %in% left))
dbDisconnect(con)

# ======================= 3. Модуль входа =======================
db_m <- temp_path("t_mod.db"); unlink(db_m)
conf <<- list(database_settings = list(app_data_base = db_m), logging = list(action_log = TRUE))
invisible(capture.output(suppressWarnings(source("create_db.R"))))
con <- dbConnect(SQLite(), db_m)
create_user(con, "alsey", "rightpass", "admin"); create_user(con, "ida", "idapass1", "viewer")
check_user <- function(l, p) authenticate_user(con, l, p)
log_count <- function(fn) dbGetQuery(con, "SELECT COUNT(*) AS n FROM action_log WHERE func_name = ?", params = list(fn))$n

logged_in <- reactiveVal(FALSE); user_role <- reactiveVal(NULL)
testServer(mod_auth_server, args = list(logged_in = logged_in, user_role = user_role, check_user_fun = check_user, conn = con), {
  msgs <- character()
  for (i in 1:5) {
    session$setInputs(login = "alsey", password = paste0("wrong", i), login_btn = i)
    msgs[i] <- output$login_message
  }
  chk("модуль: первые 4 неудачи: сообщение «Неверный логин или пароль»", all(msgs[1:4] == "Неверный логин или пароль"))
  chk("модуль: 5-я неудача: сообщение о блокировке с минутами", grepl("Слишком много неудачных попыток входа", msgs[5]) && grepl("15 мин", msgs[5]))
  session$setInputs(login = "alsey", password = "rightpass", login_btn = 6)
  chk("модуль: правильный пароль при блокировке НЕ пускает", !logged_in() && grepl("Слишком много", output$login_message))
  n_before <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM login_attempts")$n
  session$setInputs(login = "alsey", password = "wrong", login_btn = 7)
  chk("модуль: попытки во время блокировки не записываются (блокировка не продлевается)", dbGetQuery(con, "SELECT COUNT(*) AS n FROM login_attempts")$n == n_before)
  chk("модуль: в журнале действий 4 неудачи и 1 блокировка", log_count("Login failed") == 4 && log_count("Login locked") == 1)

  # блокировка истекла
  dbExecute(con, "UPDATE login_attempts SET attempted_at = datetime(attempted_at, '-16 minutes')")
  session$setInputs(login = "alsey", password = "rightpass", login_btn = 8)
  chk("модуль: после окончания блокировки правильный пароль пускает", logged_in() && user_role() == "admin")
  chk("модуль: вход записан как успешная попытка", dbGetQuery(con, "SELECT COUNT(*) AS n FROM login_attempts WHERE login = 'alsey' AND success = 1")$n == 1)
})

# другой логин, несуществующий логин и одинаковые сообщения (нельзя узнать существует ли логин)
logged_in <- reactiveVal(FALSE); user_role <- reactiveVal(NULL)
dbExecute(con, "DELETE FROM login_attempts")
testServer(mod_auth_server, args = list(logged_in = logged_in, user_role = user_role, check_user_fun = check_user, conn = con), {
  session$setInputs(login = "ida", password = "bad", login_btn = 1); m_known <- output$login_message
  session$setInputs(login = "nobody", password = "bad", login_btn = 2); m_unknown <- output$login_message
  chk("модуль: сообщение для существующего и несуществующего логина одинаковое", identical(m_known, m_unknown))
  for (i in 3:6) session$setInputs(login = "nobody", password = "bad", login_btn = i)
  chk("модуль: несуществующий логин после 5 попыток тоже блокируется", grepl("Слишком много неудачных", output$login_message))
  session$setInputs(login = "ida", password = "idapass1", login_btn = 20)
  chk("модуль: блокировка одного логина не мешает входу другого", logged_in() && user_role() == "viewer")
})

# общий лимит
logged_in <- reactiveVal(FALSE); user_role <- reactiveVal(NULL)
dbExecute(con, "DELETE FROM login_attempts")
for (i in 1:30) add_attempt(con, paste0("spray", i), FALSE, seconds_ago = 3)
testServer(mod_auth_server, args = list(logged_in = logged_in, user_role = user_role, check_user_fun = check_user, conn = con), {
  session$setInputs(login = "alsey", password = "rightpass", login_btn = 1)
  chk("модуль: при общем лимите вход временно закрыт для всех", !logged_in() && grepl("Повторите через минуту", output$login_message))
})

# защита не должна ломать вход, если таблицы попыток нет (fail-open)
logged_in <- reactiveVal(FALSE); user_role <- reactiveVal(NULL)
dbExecute(con, "DROP TABLE login_attempts")
testServer(mod_auth_server, args = list(logged_in = logged_in, user_role = user_role, check_user_fun = check_user, conn = con), {
  suppressMessages(session$setInputs(login = "alsey", password = "rightpass", login_btn = 1))
  chk("модуль: без таблицы попыток вход по правильному паролю продолжает работать", logged_in())
})

# первый запуск (нет пользователей) не затронут
db_e <- temp_path("t_empty.db"); unlink(db_e)
conf <<- list(database_settings = list(app_data_base = db_e), logging = list(action_log = TRUE))
invisible(capture.output(suppressWarnings(source("create_db.R"))))
con_e <- dbConnect(SQLite(), db_e)
logged_in <- reactiveVal(FALSE); user_role <- reactiveVal(NULL)
testServer(mod_auth_server, args = list(logged_in = logged_in, user_role = user_role, check_user_fun = function(l, p) authenticate_user(con_e, l, p), conn = con_e), {
  chk("модуль: при пустой базе показывается создание администратора", grepl("Создание администратора", as.character(output$auth_ui$html)))
  session$setInputs(setup_login = "boss", setup_password = "abcdef", setup_password_confirm = "abcdef", setup_btn = 1)
  chk("модуль: первый администратор создаётся и входит", logged_in() && user_role() == "admin")
})
dbDisconnect(con_e)

# ======================= 4. Кнопка «Снять блокировку» (вкладка «Доступ») =======================
conf <<- list(database_settings = list(app_data_base = db_m), logging = list(action_log = TRUE))
# create_db.R выше перезаписал переменную con и закрыл соединение, открываем заново
con <- dbConnect(SQLite(), db_m)
dbExecute(con, "DROP TABLE IF EXISTS login_attempts"); ensure_login_attempts_table(con)
source("create_config.R")
conf_rv <- reactiveVal(default_config())   # права по умолчанию, рабочий config.yaml не нужен
mk_auth <- function(role) list(user = reactiveVal(list(login = "tester_admin", role = role)))
for (i in 1:5) record_login_attempt(con, "ida", FALSE)
chk("перед проверкой кнопки логин ida заблокирован", login_lock_remaining(con, "ida") > 0)

unlock_try <- function(role) {
  testServer(mod_access_server, args = list(conn = con, auth = mk_auth(role), session_id = "t", conf_rv = conf_rv), {
    # у обработчика карточки ignoreInit = TRUE: сначала даём модулю инициализироваться,
    # иначе первое значение input считается начальным и обработчик не вызывается
    session$flushReact()
    session$setInputs(users_table_cell_clicked = list(username = "ida", value = "dblclick", timestamp = 1))
    session$setInputs(modal_unlock_login = 1)
  })
}
unlock_try("user")
chk("кнопка снятия блокировки: роль user НЕ может снять блокировку", login_lock_remaining(con, "ida") > 0)
unlock_try("viewer")
chk("кнопка снятия блокировки: роль viewer НЕ может снять блокировку", login_lock_remaining(con, "ida") > 0)
before_unlock_log <- log_count("Login unlock")
unlock_try("admin")
chk("кнопка снятия блокировки: admin снимает блокировку", login_lock_remaining(con, "ida") == 0)
chk("кнопка снятия блокировки: действие записано в журнал", log_count("Login unlock") == before_unlock_log + 1)

dbDisconnect(con)
unlink(c(db_new, db_old, db, db_m, db_e))
finish_tests()
