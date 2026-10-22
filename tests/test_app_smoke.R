# Сквозной тест на реальном app.R: запуск с нуля, первый администратор, блокировка
# после неудачных попыток входа, ручная блокировка, принудительное закрытие
# открытой сессии, вход по cookie автовхода.
#
# app.R запускается во ВРЕМЕННОЙ копии проекта, рабочие файлы (app.db, config.yaml,
# app.Rout) не затрагиваются. Запуск из корня проекта: Rscript tests/test_app_smoke.R

proj <- normalizePath(getwd(), winslash = "/")
libs <- .libPaths()          # библиотеки проекта (renv) нужны и во временной копии
source(file.path(proj, "tests/helper.R"))

tmp <- file.path(Sys.getenv("TEMP", tempdir()), "smoke_proj"); unlink(tmp, recursive = TRUE); dir.create(tmp)
for (f in c("app.R", "app_info.yml", "create_config.R", "create_db.R", "shiny_task_service_manager.Rproj"))
  file.copy(file.path(proj, f), tmp)
for (d in c("R", "modules", "ai_docs", "www")) file.copy(file.path(proj, d), tmp, recursive = TRUE)
setwd(tmp); .libPaths(libs)
suppressMessages({library(shiny); library(DBI); library(appauth)})

chk("временная копия: ни app.db, ни config.yaml ещё нет", !file.exists("app.db") && !file.exists("config.yaml"))

# app.R без блока sink (он пишет в app.Rout), без определения ветки через git,
# фоновые процессы и запросы к планировщику заменены заглушками
txt <- readLines("app.R", encoding = "UTF-8")
s <- grep("^if \\(!interactive\\(\\)\\) \\{", txt)[1]; e <- s + which(txt[(s + 1):length(txt)] == "}")[1]
txt <- txt[-(s:e)]
k <- grep('^if \\(system\\("git rev-parse', txt)[1]; txt <- c(txt[seq_len(k - 1)], "shinyApp(ui, server)")
future_promise <- function(expr, ...) promises::promise_resolve(NULL)
plan <- function(...) invisible(NULL)
# app.R загружается в СВОЁ окружение, как это делает runApp(): при загрузке в глобальное окружение пропадают ошибки
# вида «функция из R/ не видит переменную, созданную в app.R» (так однажды сломалась вкладка «Доступ»)
app_env <- new.env(parent = globalenv())
app_obj <- suppressWarnings(suppressMessages(eval(parse(text = txt), envir = app_env)))   # не печатаем: print() запускает приложение
server <- app_env$server
chk("app.R загружен, функция server найдена", is.function(server))
chk("первый запуск: config.yaml создан приложением", file.exists("config.yaml"))
cfg <- yaml::read_yaml("config.yaml")
chk("config.yaml содержит раздел security и право 'Просмотр кода'", all(c("max_failed_logins", "lockout_minutes") %in% names(cfg$security)) && "Просмотр кода" %in% names(cfg$access_managemet))
chk("первый запуск: app.db создан приложением", file.exists("app.db"))
con <- dbConnect(RSQLite::SQLite(), "app.db")
quiet <- function(x) invisible(x)
chk("в новой базе есть login_attempts, колонка users.blocked и действия входа и блокировки",
    "login_attempts" %in% dbListTables(con) && "blocked" %in% dbGetQuery(con, "SELECT name FROM pragma_table_info('users')")$name &&
      all(c("Login failed", "Login locked", "Login unlock", "User block", "User unblock", "Blocked login attempt") %in% dbGetQuery(con, "SELECT key FROM actions")$key))
chk("в новой базе нет пользователей (root/root не создаётся)", dbGetQuery(con, "SELECT COUNT(*) n FROM users")$n == 0)

# 1. первый администратор
testServer(server, {
  chk("пустая база: на экране создание администратора", grepl("Создание администратора", as.character(output$`auth-form`$html)))
  session$setInputs(`auth-setup_login` = "boss", `auth-setup_password` = "bosspass1", `auth-setup_confirm` = "bosspass1", `auth-setup_btn` = 1)
  chk("первый администратор создан и вошёл", logged_in() && user_role() == "admin")
  session$flushReact()
  access_html <- tryCatch(as.character(output$`access-users-main`$html), error = function(e) "")
  chk("вкладка «Доступ»: серверная часть зарегистрирована администратору, таблица пользователей и кнопка добавления есть", grepl("access-users-table", access_html, fixed = TRUE) && grepl("access-users-add", access_html, fixed = TRUE))
  chk("вкладка «Доступ»: редактор прав, переключатели логов и путей к базам отрисованы", all(vapply(c("access-role_editor", "access-logging_editor", "access-database_editor"), function(id) !inherits(tryCatch(output[[id]], error = function(e) e), "error"), logical(1))))
  app_con_ref <<- app_con                    # для проверки закрытия подключения в конце теста
  quiet(aa_create_user(con, "worker", "workpass1", "user")); quiet(aa_create_user(con, "other", "otherpass1", "user"))
  session$setInputs(logout_btn = 1)
  chk("выход выполнен", !logged_in())
})

# 2. блокировка после неудачных попыток
testServer(server, {
  for (i in 1:5) session$setInputs(`auth-login` = "worker", `auth-password` = paste0("bad", i), `auth-login_btn` = i)
  chk("5 неверных паролей: показано сообщение о блокировке", grepl("Слишком много неудачных попыток", output$`auth-message`))
  session$setInputs(`auth-login` = "worker", `auth-password` = "workpass1", `auth-login_btn` = 6)
  chk("правильный пароль заблокированного логина не пускает", !logged_in())
  session$setInputs(`auth-login` = "boss", `auth-password` = "bosspass1", `auth-login_btn` = 7)
  chk("другой логин (админ) входит как обычно", logged_in() && user_role() == "admin")
})
chk("в таблице login_attempts записаны попытки", dbGetQuery(con, "SELECT COUNT(*) n FROM login_attempts WHERE login = 'worker' AND success = 0")$n == 5)
quiet(dbExecute(con, "DELETE FROM login_attempts WHERE login = 'worker'"))

# 3. ручная блокировка: вход и открытая сессия
testServer(server, {
  session$setInputs(`auth-login` = "worker", `auth-password` = "workpass1", `auth-login_btn` = 1)
  chk("worker входит", logged_in() && user_role() == "user")
  session$elapse(31000)
  chk("пока пользователя не заблокировали, сессия остаётся открытой", logged_in())
  quiet(aa_set_blocked(con, "worker", TRUE))
  session$elapse(31000)
  chk("после блокировки открытая сессия закрывается при ближайшей проверке", !logged_in() && is.null(auth$user()) && is.null(user_role()))
})
testServer(server, {
  session$setInputs(`auth-login` = "worker", `auth-password` = "workpass1", `auth-login_btn` = 1)
  chk("заблокированный вручную не входит даже с верным паролем", !logged_in() && grepl("заблокирована администратором", output$`auth-message`))
})

# 4. вход по cookie автовхода
raw_token <- function() openssl::base64_encode(openssl::rand_bytes(32))
add_cookie_token <- function(login, raw) quiet(dbExecute(con, "INSERT INTO auth_sessions (user_login, token_hash, expires_at) VALUES (?, ?, datetime('now', '+30 days'))", params = list(login, openssl::sha256(raw))))
t_other <- raw_token(); add_cookie_token("other", t_other)
t_worker <- raw_token(); add_cookie_token("worker", t_worker)
testServer(server, {
  session$setInputs(`auth-cookie` = t_other)
  chk("cookie: обычный пользователь входит автоматически", logged_in() && user_role() == "user")
})
testServer(server, {
  session$setInputs(`auth-cookie` = t_worker)
  chk("cookie: заблокированный пользователь автоматически НЕ входит", !logged_in())
})
quiet(aa_set_blocked(con, "worker", FALSE))
t_worker2 <- raw_token(); add_cookie_token("worker", t_worker2)
testServer(server, {
  session$setInputs(`auth-cookie` = t_worker2)
  chk("cookie: после разблокировки автовход снова работает", logged_in() && user_role() == "user")
})

# 5. удалённый пользователь тоже теряет открытую сессию
testServer(server, {
  session$setInputs(`auth-login` = "other", `auth-password` = "otherpass1", `auth-login_btn` = 1)
  chk("other входит", logged_in())
  quiet(dbExecute(con, "DELETE FROM users WHERE login = 'other'"))
  session$elapse(31000)
  chk("после удаления пользователя его открытая сессия закрывается", !logged_in())
})

# 6. кнопки личного кабинета: смена своего пароля и выход на всех устройствах
quiet(aa_create_user(con, "cabinet", "cabpass1", "user"))
dev2 <- aa_issue_token(con, "cabinet")           # «другое устройство»
active_tokens <- function() dbGetQuery(con, "SELECT COUNT(*) n FROM auth_sessions WHERE user_login = 'cabinet' AND revoked = 0")$n
testServer(server, {
  session$setInputs(`auth-login` = "cabinet", `auth-password` = "cabpass1", `auth-login_btn` = 1)
  chk("cabinet входит", logged_in())
  session$setInputs(change_password_btn = 1)
  session$setInputs(`password-old` = "cabpass1", `password-new` = "cabinet-new-1", `password-confirm` = "cabinet-new-1", `password-change` = 1)
  chk("смена своего пароля: новый пароль подходит, старый нет, сессия осталась открытой",
      !is.null(aa_authenticate(con, "cabinet", "cabinet-new-1")) && is.null(aa_authenticate(con, "cabinet", "cabpass1")) && logged_in())
  chk("смена своего пароля: токен другого устройства отозван, токен этой сессии действует",
      is.null(aa_find_session_user(con, dev2)) && active_tokens() == 1)
  dev3 <- aa_issue_token(con, "cabinet")
  session$setInputs(logout_all_btn = 1)
  chk("«Выйти на всех устройствах»: до подтверждения пользователь остаётся в приложении", logged_in() && active_tokens() == 2)
  session$setInputs(logout_all_confirm = 1)
  chk("«Выйти на всех устройствах»: после подтверждения выход и все токены отозваны", !logged_in() && active_tokens() == 0 && is.null(aa_find_session_user(con, dev3)))
})

dbDisconnect(con)   # собственное подключение теста

# После закрытия всех сессий приложение не должно держать файл базы: раньше подключение
# app_con не закрывалось, и app.db нельзя было переименовать или удалить, пока жив процесс R.
chk("после закрытия сессий подключение приложения к app.db закрыто", exists("app_con_ref") && !DBI::dbIsValid(app_con_ref))
moved <- file.rename("app.db", "app_moved.db")
chk("после закрытия сессий файл app.db можно переименовать (он не занят)", isTRUE(moved) && file.exists("app_moved.db") && !file.exists("app.db"))
chk("и снова вернуть имя", file.rename("app_moved.db", "app.db") && file.exists("app.db"))
setwd(Sys.getenv("TEMP", tempdir())); unlink(tmp, recursive = TRUE)
finish_tests()
