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
  chk("вкладка «Доступ»: редактор настроек входа отрисован", !inherits(tryCatch(output[["access-security_editor"]], error = function(e) e), "error"))

  # --- настройки приложения на вкладке «Доступ»: реальные обработчики сохранения ---------
  source(file.path(proj, "R/access_settings.R"), local = TRUE)
  n_save <- 0L
  press <- function(button) { n_save <<- n_save + 1L; do.call(session$setInputs, stats::setNames(list(n_save), paste0("access-", button))) }
  set_inputs <- function(prefix, values) do.call(session$setInputs, stats::setNames(unname(values), paste0("access-", prefix, names(values))))
  cfg_now <- function() yaml::read_yaml("config.yaml")
  last_log <- function(func) dbGetQuery(con, "SELECT value FROM action_log WHERE func_name = ? ORDER BY rowid DESC LIMIT 1", params = list(func))$value

  # безопасность входа: неверное значение не записывается, верное применяется сразу и попадает в лог
  sec <- default_section("security")
  set_inputs("sec_", utils::modifyList(sec, list(max_failed_logins = 0))); press("save_security")
  chk("настройки входа: неверное значение (0 попыток) не записано в config.yaml", cfg_now()$security$max_failed_logins == 5 && get("auth_settings", globalenv())$max_failed_logins == 5)
  set_inputs("sec_", utils::modifyList(sec, list(max_failed_logins = 3, password_policy = "reject"))); press("save_security")
  chk("настройки входа: верные значения записаны в config.yaml", cfg_now()$security$max_failed_logins == 3 && cfg_now()$security$password_policy == "reject")
  chk("настройки входа: новые сессии получают новые настройки сразу", get("auth_settings", globalenv())$max_failed_logins == 3 && get("auth_settings", globalenv())$password_policy == "reject")
  chk("настройки входа: изменение записано в лог действий с прежним и новым значением", grepl("max_failed_logins: 5 -> 3", last_log("Security config change"), fixed = TRUE))
  n0 <- dbGetQuery(con, "SELECT COUNT(*) n FROM action_log WHERE func_name = 'Security config change'")$n
  press("save_security")
  chk("настройки входа: повторное сохранение без изменений ничего не пишет", dbGetQuery(con, "SELECT COUNT(*) n FROM action_log WHERE func_name = 'Security config change'")$n == n0)
  press("reset_security"); press("confirm_reset")
  chk("настройки входа: «По умолчанию» возвращает значения и пересоздаёт настройки", cfg_now()$security$max_failed_logins == 5 && cfg_now()$security$password_policy == "warn" && get("auth_settings", globalenv())$password_policy == "warn")

  # журналы
  logs <- stats::setNames(as.list(rep(TRUE, length(default_section("logging")))), names(default_section("logging")))
  set_inputs("log_", utils::modifyList(logs, list(cmd_log = FALSE))); press("save_logging")
  chk("журналы: выключенный журнал записан, остальные включены", isFALSE(cfg_now()$logging$cmd_log) && all(unlist(cfg_now()$logging[setdiff(names(logs), "cmd_log")])))
  chk("журналы: изменение записано в лог действий", grepl("cmd_log: TRUE -> FALSE", last_log("Logging config change"), fixed = TRUE))
  press("reset_logging"); press("confirm_reset")
  chk("журналы: «По умолчанию» включает все журналы", all(unlist(cfg_now()$logging)))

  # права ролей: матрица
  perms_now <- names(cfg_now()$access_managemet); roles_now <- auth_settings$roles
  grid <- list()
  for (i in seq_along(perms_now)) for (j in seq_along(roles_now)) grid[[matrix_input_id(i, j)]] <- roles_now[j] %in% cfg_now()$access_managemet[[perms_now[i]]]
  i_tasks <- match("Задачи", perms_now); i_access <- match("Доступ", perms_now)
  grid[[matrix_input_id(i_tasks, match("viewer", roles_now))]] <- FALSE
  grid[[matrix_input_id(i_access, match("admin", roles_now))]] <- FALSE   # попытка отнять у администратора вкладку «Доступ»
  set_inputs("", grid); press("save_roles")
  chk("права: роль снята у выбранного права", !"viewer" %in% cfg_now()$access_managemet[["Задачи"]] && "user" %in% cfg_now()$access_managemet[["Задачи"]])
  chk("права: у администратора вкладку «Доступ» отнять нельзя", "admin" %in% cfg_now()$access_managemet[["Доступ"]])
  chk("права: изменение записано в лог действий", grepl("Задачи", last_log("Access config change"), fixed = TRUE))
  grid[[matrix_input_id(i_tasks, match("viewer", roles_now))]] <- TRUE
  set_inputs("", grid); press("save_roles")
  chk("права: роль можно вернуть", "viewer" %in% cfg_now()$access_managemet[["Задачи"]])

  # базы данных: путь проверяется, смена требует подтверждения и вступает в силу после перезапуска
  source(file.path(proj, "R/config_backups.R"), local = TRUE)
  same_path <- function(a, b) identical(full_database_path(a), full_database_path(b))
  db_old <- cfg_now()$database_settings
  running_before <- get("conf", globalenv())$database_settings
  other_db <- file.path(tempdir(), "other_task_base.db"); file.create(other_db)
  set_inputs("db_", utils::modifyList(db_old, list(task_log_base = file.path(tempdir(), "no_such_file.db")))); press("save_database"); press("confirm_database")
  chk("базы: для базы бота несуществующий файл не записывается даже с подтверждением", same_path(cfg_now()$database_settings$task_log_base, db_old$task_log_base))
  set_inputs("db_", utils::modifyList(db_old, list(task_log_base = other_db))); press("save_database")
  chk("базы: до подтверждения путь не меняется", same_path(cfg_now()$database_settings$task_log_base, db_old$task_log_base))
  press("confirm_database")
  chk("базы: после подтверждения новый путь записан в config.yaml", same_path(cfg_now()$database_settings$task_log_base, other_db))
  chk("базы: работающее приложение остаётся на прежних базах до перезапуска", identical(get("conf", globalenv())$database_settings, running_before))
  chk("базы: изменение записано в лог действий", grepl("task_log_base", last_log("Database config change"), fixed = TRUE))
  chk("базы: подтверждение без запроса ничего не меняет", { press("confirm_database"); same_path(cfg_now()$database_settings$task_log_base, other_db) })

  new_app_db <- file.path(tempdir(), "app_fresh.db")
  # путь, введённый вручную в отдельное поле, важнее выбранного в списке
  set_inputs("db_", cfg_now()$database_settings); session$setInputs(`access-dbtxt_app_data_base` = new_app_db)
  press("save_database"); press("confirm_database"); session$setInputs(`access-dbtxt_app_data_base` = "")
  chk("базы: для базы приложения допускается ещё не созданный файл (при запуске создастся с нуля)", same_path(cfg_now()$database_settings$app_data_base, new_app_db) && !file.exists(new_app_db))
  chk("базы: приложение продолжает работать со старой базой до перезапуска", identical(get("conf", globalenv())$database_settings$app_data_base, running_before$app_data_base))
  chk("базы: при запуске по такому пути база была бы создана с нуля", database_needs_creation(cfg_now()$database_settings$app_data_base))

  fresh_db <- file.path(tempdir(), "app_created_now.db")
  session$setInputs(`access-newdb_path` = fresh_db); press("create_db_now")
  chk("базы: «Создать базу» сразу создаёт файл с таблицами, рабочие настройки не меняются", database_has_schema(fresh_db) && identical(get("conf", globalenv())$database_settings, running_before))
  chk("базы: создание записано в лог действий", grepl("создана база", last_log("Database config change"), fixed = TRUE))
  mt <- file.mtime(fresh_db); press("create_db_now")
  chk("базы: повторное создание поверх существующего файла ничего не меняет", identical(file.mtime(fresh_db), mt) && database_has_schema(fresh_db))
  session$setInputs(`access-newdb_path` = "bad_name.txt"); press("create_db_now")
  chk("базы: файл с неверным расширением не создаётся", !file.exists(file.path(getwd(), "bad_name.txt")))

  # возврат прежних путей через тот же интерфейс
  set_inputs("db_", db_old); press("save_database"); press("confirm_database")
  chk("базы: прежние пути возвращаются через интерфейс", same_path(cfg_now()$database_settings$app_data_base, db_old$app_data_base) && same_path(cfg_now()$database_settings$task_log_base, db_old$task_log_base))

  # журналы действуют сразу: их читает общая conf, из которой пишет write_action_log
  chk("журналы: общая conf обновляется при сохранении (для функций записи)", {
    set_inputs("log_", utils::modifyList(logs, list(action_log = FALSE))); press("save_logging")
    isFALSE(get("conf", globalenv())$logging$action_log)
  })
  n_probe <- dbGetQuery(con, "SELECT COUNT(*) n FROM action_log")$n
  write_action_log(user = "probe", func = "Probe", session_id = "t")
  chk("журналы: выключенный журнал действий перестаёт пополняться сразу", dbGetQuery(con, "SELECT COUNT(*) n FROM action_log")$n == n_probe)
  set_inputs("log_", logs); press("save_logging")
  write_action_log(user = "probe", func = "Probe", session_id = "t")
  chk("журналы: включённый журнал снова пишет", dbGetQuery(con, "SELECT COUNT(*) n FROM action_log WHERE func_name = 'Probe'")$n == 1)

  # резервные копии конфига
  chk("копии: перед изменениями настроек появились копии конфига", nrow(list_config_backups()) >= 1)
  set_inputs("sec_", utils::modifyList(default_section("security"), list(max_failed_logins = 7))); press("save_security")
  newest <- list_config_backups()$file[1]
  chk("копии: в последней копии прежнее значение (5), в конфиге новое (7)", read_config_backup(newest)$security$max_failed_logins == 5 && cfg_now()$security$max_failed_logins == 7)
  db_before_restore <- cfg_now()$database_settings
  session$setInputs(`access-backup_choice` = newest); press("restore_backup"); press("confirm_restore")
  chk("копии: восстановление возвращает настройки входа и обновляет работающие настройки", cfg_now()$security$max_failed_logins == 5 && get("auth_settings", globalenv())$max_failed_logins == 5 && get("conf", globalenv())$security$max_failed_logins == 5)
  chk("копии: пути к базам при восстановлении не меняются", identical(cfg_now()$database_settings, db_before_restore))
  chk("копии: восстановление записано в лог, перед ним текущий конфиг сохранён", last_log("Config restore") == newest && nrow(list_config_backups()) >= 2)
  yaml_before <- tools::md5sum("config.yaml")
  session$setInputs(`access-backup_choice` = "../config.yaml"); press("restore_backup"); press("confirm_restore")
  chk("копии: имя с .. отклоняется, конфиг не меняется", identical(tools::md5sum("config.yaml"), yaml_before))
  press("confirm_restore")
  chk("копии: подтверждение без запроса ничего не меняет", identical(tools::md5sum("config.yaml"), yaml_before))

  app_con_ref <<- app_con                   # для проверки закрытия подключения в конце теста
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
