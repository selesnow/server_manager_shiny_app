# Тесты связки приложения с пакетом авторизации appauth.
#
# Сами правила (пароли, блокировки, токены, сессии, права, модули входа и
# пользователей) проверяет пакет своими автотестами. Здесь только то, что
# относится к приложению: миграция рабочих баз, настройки из config.yaml,
# превращение событий пакета в записи лога с прежними названиями действий,
# обёртки прав, вкладка «Доступ» и оформление.
#
# Запуск из корня проекта: Rscript tests/test_appauth_integration.R
# Тесты работают с временными базами, рабочие app.db и config.yaml не изменяются.

suppressMessages({library(shiny); library(DBI); library(RSQLite); library(dplyr); library(tibble); library(lubridate); library(appauth)})
source("tests/helper.R")
for (f in c("R/actions.R", "R/logging.R", "R/access.R", "R/auth_events.R", "create_config.R")) source(f)
source("modules/mod_tab_access.R")

proj <- getwd()
quiet <- function(x) invisible(x)

# ======================= 1. Рабочие базы подключаются без потери данных =======================
# «База как на проде»: users с id и хешами, есть токены, нет blocked и попыток входа
db_prod <- temp_path("ai_prod.db"); unlink(db_prod)
prod <- dbConnect(SQLite(), db_prod)
dbExecute(prod, "CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, login TEXT UNIQUE NOT NULL, password TEXT NOT NULL, role TEXT NOT NULL)")
dbExecute(prod, "CREATE TABLE auth_sessions (id INTEGER PRIMARY KEY AUTOINCREMENT, user_login TEXT NOT NULL, token_hash TEXT NOT NULL, created_at DATETIME DEFAULT CURRENT_TIMESTAMP, expires_at DATETIME NOT NULL, revoked INTEGER DEFAULT 0)")
for (u in list(c("alsey", "admin"), c("clarkson", "user"), c("ida", "viewer")))
  dbExecute(prod, "INSERT INTO users (login, password, role) VALUES (?, ?, ?)", params = list(u[1], aa_hash_password("secret1"), u[2]))
# токен, выданный старым приложением: sha256 строки в виде текста, как делал openssl::sha256()
old_raw <- openssl::base64_encode(openssl::rand_bytes(32))
dbExecute(prod, "INSERT INTO auth_sessions (user_login, token_hash, expires_at) VALUES ('alsey', ?, datetime('now', '+10 days'))", params = list(as.character(openssl::sha256(old_raw))))
users_before <- dbGetQuery(prod, "SELECT id, login, password, role FROM users ORDER BY id")

s0 <- build_auth_settings(NULL)
chk("база как на проде: до подготовки нет login_attempts и колонки blocked", !"login_attempts" %in% dbListTables(prod) && !"blocked" %in% dbGetQuery(prod, "SELECT name FROM pragma_table_info('users')")$name)
chk("aa_prepare_db на базе как на проде завершилась без ошибок", isTRUE(suppressMessages(aa_prepare_db(prod, s0))))
chk("схема достроена: login_attempts, blocked, must_change", "login_attempts" %in% dbListTables(prod) && all(c("blocked", "must_change") %in% dbGetQuery(prod, "SELECT name FROM pragma_table_info('users')")$name))
chk("логины, хеши паролей и роли не изменились", identical(users_before, dbGetQuery(prod, "SELECT id, login, password, role FROM users ORDER BY id")))
chk("вход существующих пользователей работает после миграции", !is.null(aa_authenticate(prod, "alsey", "secret1")) && !is.null(aa_authenticate(prod, "ida", "secret1")))
chk("токен автовхода, выданный старым приложением, продолжает работать", identical(aa_find_session_user(prod, old_raw)$login, "alsey"))
chk("повторная подготовка безопасна и ничего не меняет", isTRUE(suppressMessages(aa_prepare_db(prod, s0))) && identical(users_before, dbGetQuery(prod, "SELECT id, login, password, role FROM users ORDER BY id")))
dbDisconnect(prod)

# схема dev: login это PRIMARY KEY, колонки id нет
db_dev <- temp_path("ai_dev.db"); unlink(db_dev)
dv <- dbConnect(SQLite(), db_dev)
dbExecute(dv, "CREATE TABLE users (login TEXT PRIMARY KEY, password TEXT, role TEXT)")
dbExecute(dv, "INSERT INTO users VALUES ('alsey', ?, 'admin')", params = list(aa_hash_password("secret1")))
chk("схема без колонки id: подготовка проходит, пользователь входит", isTRUE(suppressMessages(aa_prepare_db(dv, s0))) && !is.null(aa_authenticate(dv, "alsey", "secret1")))
dbDisconnect(dv)

# create_db.R для новой установки: все таблицы есть, пользователей нет
tmp_new <- file.path(Sys.getenv("TEMP", tempdir()), "ai_newdb"); unlink(tmp_new, recursive = TRUE); dir.create(tmp_new)
file.copy(c("create_db.R", "create_config.R"), tmp_new); file.copy("R", tmp_new, recursive = TRUE)
owd <- setwd(tmp_new)
suppressMessages({ conf <<- ensure_config("config.yaml"); quiet(capture.output(source("create_db.R"))) })
newcon <- dbConnect(SQLite(), conf$database_settings$app_data_base)
chk("create_db.R: в новой базе есть таблицы авторизации и колонки blocked, must_change",
    all(c("users", "auth_sessions", "login_attempts", "appauth_migrations") %in% dbListTables(newcon)) &&
      all(c("blocked", "must_change") %in% dbGetQuery(newcon, "SELECT name FROM pragma_table_info('users')")$name))
chk("create_db.R: пользователей по умолчанию нет", dbGetQuery(newcon, "SELECT COUNT(*) n FROM users")$n == 0)
dbDisconnect(newcon); setwd(owd); unlink(tmp_new, recursive = TRUE)

# ======================= 2. Настройки авторизации из config.yaml =======================
d <- default_config()
sd <- build_auth_settings(NULL)
chk("без раздела security: значения по умолчанию из пакета", sd$max_failed_logins == 5 && sd$lockout_minutes == 15 && sd$max_failed_logins_per_minute == 30)
s1 <- build_auth_settings(list(max_failed_logins = 3.0, lockout_minutes = 10.0, max_failed_logins_per_minute = 7.0))
chk("значения из config.yaml применяются (числа вида 3.0 тоже)", s1$max_failed_logins == 3 && s1$lockout_minutes == 10 && s1$max_failed_logins_per_minute == 7)
chk("настройки по умолчанию из default_config() принимаются пакетом", inherits(build_auth_settings(d$security), "aa_settings") && isTRUE(build_auth_settings(d$security)$must_change_default))
chk("must_change_default можно выключить в config.yaml", isFALSE(build_auth_settings(list(must_change_default = FALSE))$must_change_default))
msgs <- character(); s2 <- withCallingHandlers(build_auth_settings(list(max_failed_logins = 0)), message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
chk("некорректный раздел security не роняет приложение: предупреждение и значения по умолчанию", s2$max_failed_logins == 5 && any(grepl("некорректный раздел security", msgs)))
chk("неизвестные ключи в security игнорируются", build_auth_settings(list(unknown_key = 1, max_failed_logins = 4))$max_failed_logins == 4)
chk("тексты на русском", identical(attr(sd$texts, "lang"), "ru") && sd$texts$login_button == "Войти")

# ======================= 3. События пакета -> записи лога =======================
# временная база с таблицами логов, conf указывает на неё
db_log <- temp_path("ai_log.db"); unlink(db_log)
lg <- dbConnect(SQLite(), db_log)
dbExecute(lg, "CREATE TABLE action_log (id INTEGER PRIMARY KEY AUTOINCREMENT, datetime DATETIME, func_name TEXT, user TEXT, session_id TEXT, value TEXT)")
dbExecute(lg, "CREATE TABLE session_log (id INTEGER PRIMARY KEY AUTOINCREMENT, datetime DATETIME, session_id TEXT, user TEXT, action TEXT)")
conf <<- list(logging = list(action_log = TRUE, session_log = TRUE), database_settings = list(app_data_base = db_log))
auth_settings <<- sd

last_action <- function() dbGetQuery(lg, "SELECT func_name, user, value FROM action_log ORDER BY id DESC LIMIT 1")
n_actions <- function() dbGetQuery(lg, "SELECT COUNT(*) n FROM action_log")$n

cases <- list(
  list("login_failed",         "ivan",  "",             "Login failed",         "ivan",  "ivan"),
  list("login_locked",         "ivan",  "900",          "Login locked",         "ivan",  "ivan"),
  list("login_blocked_user",   "ivan",  "",             "Blocked login attempt","ivan",  "ivan"),
  list("login_throttled",      "ivan",  "",             "Login throttled",      "ivan",  "ivan"),
  list("login_unlocked",       "alsey", "ivan",         "Login unlock",         "alsey", "ivan"),
  list("first_admin_created",  "boss",  "",             "User add",             "boss",  "boss"),
  list("user_added",           "alsey", "olga (viewer)","User add",             "alsey", "olga"),
  list("user_removed",         "alsey", "olga",         "User remove",          "alsey", "olga"),
  list("user_role_changed",    "alsey", "olga: admin",  "User change role",     "alsey", "olga"),
  list("password_reset",       "alsey", "olga",         "User password reset",  "alsey", "olga"),
  list("password_changed",     "olga",  "",             "Password change",      "olga",  ""),
  list("sessions_revoked",     "alsey", "olga",         "Sessions revoke",      "alsey", "olga"),
  list("user_blocked",         "alsey", "olga",         "User block",           "alsey", "olga"),
  list("user_unblocked",       "alsey", "olga",         "User unblock",         "alsey", "olga"),
  list("session_forced_logout","olga",  "blocked",      "Forced logout",        "olga",  "blocked"),
  list("access_denied",        "olga",  "Run",          "Access denied",        "olga",  "Run")
)
for (cs in cases) {
  log_auth_event(cs[[1]], cs[[2]], "s1", cs[[3]])
  a <- last_action()
  chk(sprintf("событие %-22s -> действие '%s' (пользователь %s, value '%s')", cs[[1]], cs[[4]], cs[[5]], cs[[6]]),
      identical(a$func_name, cs[[4]]) && identical(a$user, cs[[5]]) && identical(a$value, cs[[6]]))
}

n0 <- n_actions(); log_auth_event("login_success", "ivan", "s1", ""); log_auth_event("logout", "ivan", "s1", "")
chk("login_success и logout в лог действий не пишутся", n_actions() == n0)
chk("logout пишет конец сессии в session_log", dbGetQuery(lg, "SELECT COUNT(*) n FROM session_log WHERE user = 'ivan' AND action = 'end' AND session_id = 's1'")$n == 1)
log_auth_event("session_forced_logout", "olga", "s2", "role_changed")
chk("принудительный выход тоже пишет конец сессии", dbGetQuery(lg, "SELECT COUNT(*) n FROM session_log WHERE user = 'olga' AND action = 'end' AND session_id = 's2'")$n == 1)
s_before <- dbGetQuery(lg, "SELECT COUNT(*) n FROM session_log")$n
log_auth_event("logout", "anonymous", "s3", "")
chk("выход без пользователя (anonymous) в session_log не пишется", dbGetQuery(lg, "SELECT COUNT(*) n FROM session_log")$n == s_before)

conf$logging$action_log <- FALSE
n1 <- n_actions(); log_auth_event("user_added", "alsey", "s1", "x (user)")
chk("переключатель action_log выключен: события в лог не пишутся", n_actions() == n1)
conf$logging$action_log <- TRUE

# новые события пакета не должны остаться без решения
ignored <- c("login_success", "logout")
all_events <- aa_events()$event
chk("каждое событие пакета либо сопоставлено действию лога, либо осознанно пропущено", all(all_events %in% c(names(AUTH_EVENT_ACTIONS), ignored)))
chk("все сопоставленные названия действий есть в справочнике actions_catalog()", all(AUTH_EVENT_ACTIONS %in% actions_catalog()$key))
chk("все сопоставленные события существуют в пакете", all(names(AUTH_EVENT_ACTIONS) %in% all_events))
chk("в справочнике действий нет повторяющихся id и ключей", !anyDuplicated(actions_catalog()$id) && !anyDuplicated(actions_catalog()$key))

# ======================= 4. Права: обёртки над пакетом =======================
conf_test <- list(access_managemet = list(Задачи = c("admin", "user", "viewer"), CMD = "admin", Пусто = NULL, Лог = c("admin", "user")))
chk("user_can: роль из списка", user_can("user", "Задачи", conf_test) && user_can("admin", "CMD", conf_test))
chk("user_can: роль не из списка", !user_can("viewer", "CMD", conf_test) && !user_can("user", "CMD", conf_test))
chk("user_can: неизвестное и пустое право", !user_can("admin", "Нет такого", conf_test) && !user_can("admin", "Пусто", conf_test))
chk("user_can: пустая, NA и составная роль не получают прав", !user_can("", "Задачи", conf_test) && !user_can(NA_character_, "Задачи", conf_test) && !user_can(c("admin", "user"), "Задачи", conf_test) && !user_can(NULL, "Задачи", conf_test))
chk("user_can: конфиг без раздела прав никому ничего не даёт", !user_can("admin", "Задачи", list()))

conf_live <- reactiveVal(conf_test)
auth_adm <- list(user = function() list(login = "alsey", role = "admin"))
auth_usr <- list(user = function() list(login = "ivan", role = "user"))
# conf_live это reactiveVal: в приложении проверка идёт внутри обработчика (реактивный контекст), в тесте его заменяет isolate()
denied <- function(auth, perm) inherits(tryCatch(isolate(suppressMessages(require_permission(auth, conf_live, perm, "s9"))), error = function(e) e), "shiny.silent.error")
chk("require_permission: право есть - обработчик продолжается", !denied(auth_adm, "CMD") && !denied(auth_usr, "Лог"))
n2 <- n_actions()
chk("require_permission: права нет - обработчик останавливается", denied(auth_usr, "CMD"))
a <- last_action()
chk("require_permission: отказ записан в лог как 'Access denied' с названием права", n_actions() == n2 + 1 && a$func_name == "Access denied" && a$user == "ivan" && a$value == "CMD")
conf_live(modifyList(conf_test, list(access_managemet = list(CMD = c("admin", "user")))))
chk("матрица прав читается на ходу: после правки в редакторе доступ появился без перезапуска", !denied(auth_usr, "CMD"))
chk("admin_only_provider: только роль администратора", identical(admin_only_provider(list(role = "admin")), "Доступ") && length(admin_only_provider(list(role = "user"))) == 0 && length(admin_only_provider(list(role = NULL))) == 0)

# ======================= 5. Вкладка «Доступ» =======================
users_db <- temp_path("ai_users.db"); unlink(users_db)
uc <- dbConnect(SQLite(), users_db); aa_init_db(uc)
dbExecute(uc, "CREATE TABLE action_log (id INTEGER PRIMARY KEY AUTOINCREMENT, datetime DATETIME, func_name TEXT, user TEXT, session_id TEXT, value TEXT)")
dbExecute(uc, "CREATE TABLE session_log (id INTEGER PRIMARY KEY AUTOINCREMENT, datetime DATETIME, session_id TEXT, user TEXT, action TEXT)")
quiet(aa_create_user(uc, "boss", "bosspass1", "admin")); quiet(aa_create_user(uc, "boss2", "bosspass1", "admin")); quiet(aa_create_user(uc, "worker", "workpass1", "user"))
dbWriteTable(uc, "action_log", data.frame(datetime = "2026-01-01 10:00:00", func_name = "User add", user = "boss", session_id = "x", value = "worker"), append = TRUE)

wd_tab <- file.path(Sys.getenv("TEMP", tempdir()), "ai_tab"); unlink(wd_tab, recursive = TRUE); dir.create(wd_tab)
owd <- setwd(wd_tab)
yaml::write_yaml(d, "config.yaml", fileEncoding = "UTF-8")
conf <<- list(logging = list(action_log = TRUE, session_log = TRUE), database_settings = list(app_data_base = users_db),
              access_managemet = d$access_managemet)
auth_settings <<- sd
cfg_rv <- reactiveVal(modifyList(d, list(database_settings = list(app_data_base = users_db))))
mk_auth <- function(login, role) list(user = reactiveVal(list(login = login, role = role)))
log_n <- function(fn) dbGetQuery(uc, "SELECT COUNT(*) n FROM action_log WHERE func_name = ?", params = list(fn))$n
is_blocked <- function(l) dbGetQuery(uc, "SELECT blocked FROM users WHERE login = ?", params = list(l))$blocked == 1
run_tab <- function(auth, expr) rlang::inject(suppressMessages(testServer(mod_access_server, args = list(conn = uc, auth = auth, session_id = "s5", conf_rv = cfg_rv), expr = !!rlang::enquo(expr))))

run_tab(mk_auth("boss", "admin"), {
  session$setInputs(`users-open` = "worker"); session$setInputs(`users-block` = 1)
})
chk("вкладка: администратор блокирует пользователя через модуль пакета", is_blocked("worker"))
chk("вкладка: блокировка записана в лог действий (User block, value = логин)", log_n("User block") == 1 &&
      dbGetQuery(uc, "SELECT value FROM action_log WHERE func_name = 'User block'")$value == "worker")
run_tab(mk_auth("boss", "admin"), {
  session$setInputs(`users-open` = "worker"); session$setInputs(`users-unblock` = 1)
})
chk("вкладка: разблокировка и запись User unblock", !is_blocked("worker") && log_n("User unblock") == 1)

run_tab(mk_auth("boss", "admin"), {
  session$setInputs(`users-open` = "boss"); session$setInputs(`users-block` = 1)
})
chk("вкладка: себя заблокировать нельзя", !is_blocked("boss"))

run_tab(mk_auth("worker", "user"), {
  session$setInputs(`users-open` = "boss2"); session$setInputs(`users-block` = 1)
})
chk("вкладка: роль user не может управлять пользователями", !is_blocked("boss2"))
run_tab(mk_auth("worker", "viewer"), {
  session$setInputs(`users-open` = "boss2"); session$setInputs(`users-block` = 1)
})
chk("вкладка: роль viewer не может управлять пользователями", !is_blocked("boss2"))

# редактор прав ролей: сохраняет только админ, права читаются и из файла
before_cfg <- isolate(cfg_rv())$access_managemet$CMD
run_tab(mk_auth("worker", "user"), {
  session$setInputs(roles_CMD = c("admin", "user"), save_roles = 1)
})
chk("редактор прав: роль user не может изменить права", identical(isolate(cfg_rv())$access_managemet$CMD, before_cfg))
run_tab(mk_auth("boss", "admin"), {
  session$setInputs(roles_CMD = c("admin", "user"), save_roles = 1)
})
chk("редактор прав: администратор сохраняет права, они записаны в config.yaml", identical(isolate(cfg_rv())$access_managemet$CMD, c("admin", "user")) &&
      identical(yaml::read_yaml("config.yaml")$access_managemet$CMD, c("admin", "user")) && log_n("Access config change") == 1)

info <- as.character(user_card_log_info(uc, "worker", identity))
chk("слот карточки: дата регистрации берётся из лога действий", grepl("2026-01-01 10:00:00", info, fixed = TRUE) && grepl("Дата регистрации", info, fixed = TRUE))
info2 <- as.character(user_card_log_info(uc, "boss2", identity))
chk("слот карточки: без записей в логах понятные заглушки", grepl("Неизвестно", info2, fixed = TRUE) && grepl("Нет активности в логах", info2, fixed = TRUE))
info3 <- as.character(user_card_log_info(uc, "<img src=x onerror=alert(1)>", identity))
chk("слот карточки: логин с HTML не выводится как разметка", !grepl("<img", info3, fixed = TRUE))
setwd(owd); unlink(wd_tab, recursive = TRUE)

# ======================= 6. Блокировка по логину и старые записи попыток =======================
# в базах dev и prod до перехода на пакет попытки хранились в нижнем регистре
for (i in 1:5) dbExecute(uc, "INSERT INTO login_attempts (login, success) VALUES ('alsey', 0)")
chk("старые записи попыток в нижнем регистре продолжают блокировать точный логин", aa_login_lock_remaining(uc, "alsey", sd) > 0)
chk("теперь логин считается точно: другой регистр (ALSEY) имеет свой счётчик", aa_login_lock_remaining(uc, "ALSEY", sd) == 0)
ov <- aa_users_overview(uc, sd)
chk("таблица пользователей строится на старой и новой схеме без ошибок", is.data.frame(ov) && all(c("login", "role", "state", "status") %in% names(ov)))
dbDisconnect(uc)

# ======================= 7. Оформление и подключение =======================
css <- paste(readLines("www/css/main-styles.css", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
chk("CSS приложения: красный фон для aa-row-blocked и aa-row-locked с !important", grepl("tr\\.aa-row-blocked > td", css) && grepl("tr\\.aa-row-locked > td", css) && grepl("rgba\\(239, 68, 68, [0-9.]+\\) !important", css))
chk("CSS приложения: правил старого класса user-row-blocked не осталось", !grepl("user-row-blocked", css, fixed = TRUE))
chk("CSS приложения: есть правила светлой темы и наведения", grepl("body\\.light-mode table\\.dataTable tbody tr\\.aa-row-blocked", css) && grepl("tr\\.aa-row-blocked:hover", css))
app_txt <- paste(readLines("app.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
chk("app.R подключает стили и скрипты пакета и модуль входа пакета", grepl("aa_app_head()", app_txt, fixed = TRUE) && grepl("aa_auth_ui(\"auth\")", app_txt, fixed = TRUE))
chk("app.R: прежнее имя cookie auth_token сохранено (открытые сессии не теряются)", grepl("cookie_name = \"auth_token\"", app_txt, fixed = TRUE))
chk("app.R: подключение закрывается вместе с сессией (aa_session_connection) и работает сторож сессии", grepl("aa_session_connection(", app_txt, fixed = TRUE) && grepl("aa_session_guard(", app_txt, fixed = TRUE))
chk("app.R: самописного JS cookie и старых функций авторизации больше нет", !grepl("setAuthCookie|check_user|perform_logout|authenticate_user|find_session_user", app_txt))
chk("app.R подключает main-styles.css с обновлённым номером версии", grepl("main-styles\\.css\\?v=(1[1-9]|[2-9][0-9])", app_txt))
chk("старые файлы авторизации удалены из проекта", !any(file.exists(c("R/passwords.R", "R/login_protection.R", "R/user_blocking.R", "modules/mod_auth.R"))))
all_code <- unlist(lapply(c("app.R", list.files(c("R", "modules"), full.names = TRUE, pattern = "[.]R$")), readLines, warn = FALSE, encoding = "UTF-8"))
chk("в коде не осталось вызовов удалённых функций авторизации",
    !any(grepl("(^|[^_a-z])(hash_password|verify_password|validate_new_password|validate_new_login|create_user|set_user_password|set_user_blocked|delete_user_account|check_can_(block|delete)_user|login_lock_remaining|record_login_attempt|clear_login_attempts|users_overview|is_user_blocked|user_session_valid|normalize_login)\\(", all_code)))

finish_tests()
