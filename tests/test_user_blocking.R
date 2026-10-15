# Тесты ручной блокировки пользователей (R/user_blocking.R, modules/mod_auth.R,
# кнопки в карточке пользователя и подсветка в таблице на вкладке «Доступ»).
# Запуск из корня проекта: Rscript tests/test_user_blocking.R

suppressMessages({library(shiny); library(DBI); library(RSQLite); library(dplyr); library(tibble); library(DT)})
source("tests/helper.R")
for (f in c("R/passwords.R", "R/actions.R", "R/logging.R", "R/access.R", "R/login_protection.R", "R/user_blocking.R", "create_config.R")) source(f)
source("modules/mod_auth.R"); source("modules/mod_tab_access.R")

# результат этих функций в тестах не нужен и не должен печататься
dbExecute <- function(...) invisible(DBI::dbExecute(...))
quiet <- function(f) function(...) invisible(f(...))
create_user_q <- quiet(create_user)
set_blocked_q <- quiet(set_user_blocked)

# ======================= 1. Миграция: колонка blocked в базе «как на проде» =======================
db_old <- temp_path("tb_old.db"); unlink(db_old)
old <- dbConnect(SQLite(), db_old)
dbExecute(old, "CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, login TEXT UNIQUE NOT NULL, password TEXT NOT NULL, role TEXT NOT NULL)")
for (u in list(c("alsey", "admin"), c("clarkson", "user"), c("ida", "viewer"))) create_user_q(old, u[1], "secret1", u[2])
before <- dbGetQuery(old, "SELECT * FROM users ORDER BY id")
chk("старая база: колонки blocked нет", !"blocked" %in% dbGetQuery(old, "SELECT name FROM pragma_table_info('users')")$name)
chk("prepare_user_blocking на старой базе без ошибок", isTRUE(suppressMessages(prepare_user_blocking(old))))
after <- dbGetQuery(old, "SELECT * FROM users ORDER BY id")
chk("колонка blocked добавлена, у всех существующих пользователей 0", "blocked" %in% names(after) && all(after$blocked == 0))
chk("логины, пароли и роли не изменились", identical(before, after[, names(before)]))
chk("повторный запуск безопасен и ничего не меняет", isTRUE(suppressMessages(prepare_user_blocking(old))) && identical(after, dbGetQuery(old, "SELECT * FROM users ORDER BY id")))
chk("вход старых пользователей работает после миграции", !is.null(authenticate_user(old, "alsey", "secret1")))
dbDisconnect(old)

# старая схема dev: login PRIMARY KEY, без id
db_dev <- temp_path("tb_dev.db"); unlink(db_dev)
dv <- dbConnect(SQLite(), db_dev)
dbExecute(dv, "CREATE TABLE users (login TEXT PRIMARY KEY, password TEXT, role TEXT)")
dbExecute(dv, "INSERT INTO users VALUES ('a', 'x', 'admin')")
chk("схема без колонки id: миграция тоже проходит", isTRUE(suppressMessages(prepare_user_blocking(dv))) && dbGetQuery(dv, "SELECT blocked FROM users")$blocked == 0)
dbDisconnect(dv)

# ======================= 2. Функции блокировки =======================
db <- temp_path("tb_main.db"); unlink(db)
conf <<- list(database_settings = list(app_data_base = db), logging = list(action_log = TRUE))
invisible(capture.output(suppressWarnings(source("create_db.R"))))
con <- dbConnect(SQLite(), db)   # create_db.R перезаписал con и закрыл соединение, открываем заново
chk("create_db.R: в новой базе есть колонка blocked", "blocked" %in% dbGetQuery(con, "SELECT name FROM pragma_table_info('users')")$name)
create_user_q(con, "boss", "bosspass1", "admin"); create_user_q(con, "boss2", "bosspass2", "admin")
create_user_q(con, "worker", "workpass1", "user"); create_user_q(con, "viewer1", "viewpass1", "viewer")
add_token <- function(login, hash) dbExecute(con, "INSERT INTO auth_sessions (user_login, token_hash, expires_at) VALUES (?, ?, datetime('now', '+30 days'))", params = list(login, hash))
revoked_of <- function(hash) dbGetQuery(con, "SELECT revoked FROM auth_sessions WHERE token_hash = ?", params = list(hash))$revoked

chk("новый пользователь не заблокирован", !is_user_blocked(con, "worker"))
chk("несуществующий пользователь не считается заблокированным", !is_user_blocked(con, "ghost"))
add_token("worker", "tok_w1"); add_token("worker", "tok_w2"); add_token("boss", "tok_b1")
chk("до блокировки токен находит пользователя (автовход)", nrow(find_session_user(con, "tok_w1")) == 1)
set_blocked_q(con, "worker", TRUE)
chk("после блокировки is_user_blocked = TRUE", is_user_blocked(con, "worker"))
chk("блокировка отзывает все токены пользователя", revoked_of("tok_w1") == 1 && revoked_of("tok_w2") == 1)
chk("блокировка не трогает токены других пользователей", revoked_of("tok_b1") == 0)
chk("заблокированный не находится по токену автовхода", nrow(find_session_user(con, "tok_w1")) == 0)
add_token("worker", "tok_w3")
chk("даже новый непросроченный токен заблокированного не работает", nrow(find_session_user(con, "tok_w3")) == 0)
chk("user_session_valid: заблокированный -> FALSE", !user_session_valid(con, "worker"))
chk("user_session_valid: обычный -> TRUE", user_session_valid(con, "boss"))
chk("user_session_valid: удалённый пользователь -> FALSE", !user_session_valid(con, "ghost"))
set_blocked_q(con, "worker", FALSE)
chk("после разблокировки is_user_blocked = FALSE", !is_user_blocked(con, "worker"))
chk("после разблокировки новый токен снова работает", nrow(find_session_user(con, "tok_w3")) == 1)
chk("отозванные при блокировке токены остаются отозванными (нужен новый вход)", nrow(find_session_user(con, "tok_w1")) == 0)

# правила: кого нельзя блокировать
chk("check_can_block_user: обычного пользователя можно", is.null(check_can_block_user(con, "worker", "boss")))
chk("check_can_block_user: себя нельзя", grepl("самого себя", check_can_block_user(con, "boss", "boss")))
chk("check_can_block_user: несуществующего нельзя", grepl("не найден", check_can_block_user(con, "ghost", "boss")))
chk("check_can_block_user: пустое значение нельзя", !is.null(check_can_block_user(con, "", "boss")) && !is.null(check_can_block_user(con, NA_character_, "boss")))
chk("check_can_block_user: админа можно, пока есть другой активный админ", is.null(check_can_block_user(con, "boss2", "boss")))
set_blocked_q(con, "boss2", TRUE)
chk("check_can_block_user: последнего активного админа нельзя", grepl("последнего активного администратора", check_can_block_user(con, "boss", "worker")))
set_blocked_q(con, "boss2", FALSE)

# вход: authenticate_user и блокировка
a <- authenticate_user(con, "worker", "workpass1")
chk("authenticate_user возвращает login, role, blocked = 0", identical(names(a), c("login", "role", "blocked")) && a$blocked == 0)
set_blocked_q(con, "worker", TRUE)
a <- authenticate_user(con, "worker", "workpass1")
chk("authenticate_user: у заблокированного с верным паролем blocked = 1", !is.null(a) && a$blocked == 1)
chk("authenticate_user: у заблокированного с неверным паролем NULL (список логинов не раскрывается)", is.null(authenticate_user(con, "worker", "bad")))
set_blocked_q(con, "worker", FALSE)

# обзор пользователей
for (i in 1:5) record_login_attempt(con, "viewer1", FALSE)
set_blocked_q(con, "worker", TRUE)
ov <- users_overview(con)
chk("users_overview: колонки login, role, Статус, state", identical(names(ov), c("login", "role", "Статус", "state")))
chk("users_overview: состояния ok / blocked / locked определяются верно",
    ov$state[ov$login == "boss"] == "ok" && ov$state[ov$login == "worker"] == "blocked" && ov$state[ov$login == "viewer1"] == "locked")
chk("users_overview: тексты статусов", ov$`Статус`[ov$login == "boss"] == "" && grepl("администратором", ov$`Статус`[ov$login == "worker"]) && grepl("неудачных попыток", ov$`Статус`[ov$login == "viewer1"]))
chk("users_overview: пустая таблица не падает", { e <- dbConnect(SQLite(), ":memory:"); dbExecute(e, "CREATE TABLE users (id INTEGER PRIMARY KEY, login TEXT, role TEXT, password TEXT, blocked INTEGER)"); ensure_login_attempts_table(e); r <- users_overview(e); dbDisconnect(e); nrow(r) == 0 && identical(names(r), c("login", "role", "Статус", "state")) })
set_blocked_q(con, "worker", FALSE); invisible(clear_login_attempts(con, "viewer1"))

# ======================= 3. Модуль входа =======================
check_user <- function(l, p) authenticate_user(con, l, p)
log_count <- function(fn) dbGetQuery(con, "SELECT COUNT(*) AS n FROM action_log WHERE func_name = ?", params = list(fn))$n
set_blocked_q(con, "worker", TRUE)
logged_in <- reactiveVal(FALSE); user_role <- reactiveVal(NULL)
testServer(mod_auth_server, args = list(logged_in = logged_in, user_role = user_role, check_user_fun = check_user, conn = con), {
  session$setInputs(login = "worker", password = "workpass1", login_btn = 1)
  chk("модуль входа: заблокированный с верным паролем НЕ входит", !logged_in())
  chk("модуль входа: показано сообщение о блокировке администратором", grepl("заблокирована администратором", output$login_message))
  chk("модуль входа: попытка записана в лог (Blocked login attempt)", log_count("Blocked login attempt") == 1)
  chk("модуль входа: токен автовхода не выдан", dbGetQuery(con, "SELECT COUNT(*) AS n FROM auth_sessions WHERE user_login = 'worker' AND revoked = 0 AND token_hash NOT LIKE 'tok_%'")$n == 0)
  session$setInputs(login = "worker", password = "wrong", login_btn = 2)
  chk("модуль входа: с неверным паролем обычное сообщение (без раскрытия блокировки)", identical(output$login_message, "Неверный логин или пароль"))
  chk("модуль входа: верный пароль заблокированного не считается успехом и не сбрасывает счётчик", dbGetQuery(con, "SELECT COUNT(*) AS n FROM login_attempts WHERE login = 'worker' AND success = 1")$n == 0)
  set_blocked_q(con, "worker", FALSE)
  session$setInputs(login = "worker", password = "workpass1", login_btn = 3)
  chk("модуль входа: после разблокировки пользователь входит", logged_in() && user_role() == "user")
})

# ======================= 4. Вкладка «Доступ»: блокировка в карточке =======================
conf <<- list(database_settings = list(app_data_base = db), logging = list(action_log = TRUE))
conf_rv <- reactiveVal(default_config())
mk_auth <- function(login, role) list(user = reactiveVal(list(login = login, role = role)))
access_try <- function(login, role, target, input_name) {
  testServer(mod_access_server, args = list(conn = con, auth = mk_auth(login, role), session_id = "t", conf_rv = conf_rv), {
    session$flushReact()   # у обработчика карточки ignoreInit = TRUE
    session$setInputs(users_table_cell_clicked = list(username = target, value = "dblclick", timestamp = 1))
    do.call(session$setInputs, setNames(list(1), input_name))
  })
}
add_token("worker", "tok_w_new")

access_try("boss", "user", "worker", "modal_block_user")
chk("карточка: роль user НЕ может заблокировать", !is_user_blocked(con, "worker"))
access_try("boss", "viewer", "worker", "modal_block_user")
chk("карточка: роль viewer НЕ может заблокировать", !is_user_blocked(con, "worker"))
access_try("boss", "admin", "worker", "modal_block_user")
chk("карточка: admin блокирует пользователя", is_user_blocked(con, "worker"))
chk("карточка: при блокировке токены пользователя отозваны", revoked_of("tok_w_new") == 1)
chk("карточка: блокировка записана в лог (User block)", log_count("User block") == 1)
access_try("boss", "admin", "boss", "modal_block_user")
chk("карточка: админ НЕ может заблокировать самого себя", !is_user_blocked(con, "boss"))
set_blocked_q(con, "boss2", TRUE)
access_try("worker", "admin", "boss", "modal_block_user")
chk("карточка: последнего активного админа заблокировать нельзя", !is_user_blocked(con, "boss"))
set_blocked_q(con, "boss2", FALSE)
for (i in 1:5) record_login_attempt(con, "worker", FALSE)
access_try("boss", "user", "worker", "modal_unblock_user")
chk("карточка: роль user НЕ может разблокировать", is_user_blocked(con, "worker"))
access_try("boss", "admin", "worker", "modal_unblock_user")
chk("карточка: admin разблокирует пользователя", !is_user_blocked(con, "worker"))
chk("карточка: при разблокировке снимается и блокировка по неудачным попыткам", login_lock_remaining(con, "worker") == 0)
chk("карточка: разблокировка записана в лог (User unblock)", log_count("User unblock") == 1)

# ======================= 5. Таблица пользователей: красная подсветка =======================
set_blocked_q(con, "worker", TRUE)
for (i in 1:5) record_login_attempt(con, "viewer1", FALSE)
rendered <- NULL
testServer(mod_access_server, args = list(conn = con, auth = mk_auth("boss", "admin"), session_id = "t", conf_rv = conf_rv), {
  # отрисованная таблица - это JSON с настройками DataTables
  rendered <<- tryCatch(as.character(output$users_table), error = function(e) paste("ОШИБКА:", conditionMessage(e)))
})
chk("таблица пользователей отрисовывается без ошибок", !grepl("^ОШИБКА", rendered) && nchar(rendered) > 100)
# Фон строки нельзя задавать инлайн: тема приложения перебивает его правилом
# "tr { background-color: transparent !important }". Поэтому строке добавляется
# класс, а цвет задаётся в CSS (так пропала подсветка, когда она была инлайновой).
chk("подсветка: строкам со state blocked и locked добавляется класс user-row-blocked",
    grepl("toggleClass('user-row-blocked', data[3] === 'blocked' || data[3] === 'locked')", rendered, fixed = TRUE))
css <- readLines("www/css/main-styles.css", encoding = "UTF-8")
i_base  <- grep("^table\\.dataTable tbody tr \\{", css)[1]
i_blocked <- grep("^table\\.dataTable tbody tr\\.user-row-blocked td \\{", css)[1]
chk("CSS: правило красного фона для .user-row-blocked есть и стоит после общих правил таблиц", !is.na(i_base) && !is.na(i_blocked) && i_blocked > i_base)
blocked_rule <- paste(css[i_blocked:(i_blocked + 2)], collapse = " ")
chk("CSS: красный фон задан с !important (иначе его перебьёт тема)", grepl("rgba\\(239, 68, 68, [0-9.]+\\) !important", blocked_rule))
chk("CSS: есть правила для светлой темы и для наведения курсора",
    any(grepl("^body\\.light-mode table\\.dataTable tbody tr\\.user-row-blocked td \\{", css)) && any(grepl("^table\\.dataTable tbody tr\\.user-row-blocked:hover td \\{", css)))
app_src <- readLines("app.R", encoding = "UTF-8")
chk("app.R подключает main-styles.css с номером версии (сброс кэша браузера)", any(grepl("main-styles\\.css\\?v=[0-9]+", app_src)))
chk("колонка state скрыта, колонка «Статус» видна", grepl('{"visible":false,"targets":3}', rendered, fixed = TRUE) && grepl("Статус", rendered, fixed = TRUE))
chk("логин остаётся первой колонкой (на него опирается двойной клик по строке)", grepl("data-username', data[0]", rendered, fixed = TRUE))

dbDisconnect(con)
unlink(c(db_old, db_dev, db))
finish_tests()
