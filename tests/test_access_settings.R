# Тесты настроек на вкладке «Доступ»: раскладка прав, матрица ролей, проверка настроек
# безопасности и путей к базам, описание изменений.
# Запуск из корня проекта: Rscript tests/test_access_settings.R

source("tests/helper.R")
source("create_config.R")
source("R/access_settings.R")
suppressPackageStartupMessages(library(appauth))

d <- default_config()
perms <- names(d$access_managemet)

# ======================= раскладка прав =======================
lay <- permission_layout(perms)
chk("раскладка: все права конфига попали в группы ровно по одному разу", setequal(unlist(lay), perms) && !anyDuplicated(unlist(lay)))
chk("раскладка: вкладки и действия разнесены по двум группам", identical(names(lay), c("Вкладки приложения", "Действия внутри вкладок")))
chk("раскладка: право вне известных групп попадает в «Прочее»", identical(permission_layout(c("Задачи", "Новое право"))[["Прочее"]], "Новое право"))
chk("раскладка: пустые группы не показываются", identical(names(permission_layout("Задачи")), "Вкладки приложения"))
chk("для каждого права по умолчанию есть подсказка", all(perms %in% names(PERMISSION_HINTS)))
chk("все права из групп есть в конфиге по умолчанию (нет опечаток в названиях)", all(unlist(PERMISSION_GROUPS) %in% perms))
chk("идентификаторы матрицы уникальны и без пробелов", {
  ids <- outer(1:3, 1:3, matrix_input_id)
  !anyDuplicated(ids) && !any(grepl("[^A-Za-z0-9_]", ids))
})

# ======================= роли из матрицы =======================
roles <- c("admin", "user", "viewer")
chk("матрица: выбранные роли попадают в право", identical(roles_from_matrix(c(TRUE, TRUE, FALSE), roles, "admin", "Задачи", "admin"), c("admin", "user")))
chk("матрица: всё выключено - роль пуста", length(roles_from_matrix(c(FALSE, FALSE, FALSE), roles, "admin", "Задачи", "admin")) == 0)
chk("матрица: NULL и NA считаются выключенными", length(roles_from_matrix(NULL, roles, character(0), "Задачи", "admin")) == 0 && identical(roles_from_matrix(c(NA, TRUE, NA), roles, character(0), "Задачи", "admin"), "user"))
chk("матрица: администратор остаётся у права «Доступ», даже если его сняли", identical(roles_from_matrix(c(FALSE, TRUE, FALSE), roles, "admin", "Доступ", "admin"), c("admin", "user")))
chk("матрица: у других прав администратора снять можно", identical(roles_from_matrix(c(FALSE, TRUE, FALSE), roles, "admin", "CMD", "admin"), "user"))
chk("матрица: устаревшая роль из конфига не теряется", identical(roles_from_matrix(c(TRUE, FALSE, FALSE), roles, c("admin", "old_role"), "Задачи", "admin"), c("admin", "old_role")))
chk("матрица: повторов нет", !anyDuplicated(roles_from_matrix(c(TRUE, TRUE, TRUE), roles, c("admin", "user"), "Задачи", "admin")))

# ======================= журналы и базы: подписи =======================
chk("журналы: у всех ключей по умолчанию есть русское название и подсказка", all(vapply(names(d$logging), function(k) { m <- logging_meta(k); length(m) == 2 && m[1] != k && nzchar(m[2]) }, logical(1))))
chk("журналы: неизвестный ключ показывается как есть", identical(logging_meta("new_log"), c("new_log", "")))
chk("базы: у ключей по умолчанию есть название и подсказка", all(vapply(names(d$database_settings), function(k) { m <- database_meta(k); m[1] != k && nzchar(m[2]) }, logical(1))))

# ======================= проверка путей к базам =======================
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })
tmp_db <- tempfile(fileext = ".db"); file.create(tmp_db)
real_db <- tempfile(fileext = ".db"); local({ con <- dbConnect(SQLite(), real_db); dbExecute(con, "CREATE TABLE t (a INTEGER)"); dbDisconnect(con) })
text_file <- tempfile(fileext = ".db"); writeLines("это не база", text_file)
new_path <- tempfile(fileext = ".db")

chk("пути: существующий файл подходит", is.null(validate_database_paths(list(app_data_base = tmp_db))))
chk("пути: настоящая база SQLite подходит", is.null(validate_database_paths(list(app_data_base = real_db))))
chk("пути: текстовый файл вместо базы отклоняется", grepl("не база SQLite", validate_database_paths(list(app_data_base = text_file))))
chk("пути: несуществующий файл отклоняется по умолчанию (иначе SQLite создаст пустую базу)", grepl("не найден", validate_database_paths(list(app_data_base = new_path))))
chk("пути: для базы приложения новый файл разрешён (при запуске он создаётся с нуля)", is.null(validate_database_paths(list(app_data_base = new_path), allow_new = DATABASE_ALLOW_NEW)))
chk("пути: для базы бота новый файл не разрешён", grepl("не найден", validate_database_paths(list(task_log_base = new_path), allow_new = DATABASE_ALLOW_NEW)))
chk("пути: новый файл в несуществующей папке отклоняется", grepl("Папка не найдена", validate_database_paths(list(app_data_base = file.path(tempdir(), "no_dir", "x.db")), allow_new = DATABASE_ALLOW_NEW)))
chk("пути: новый файл с неверным расширением отклоняется", grepl("расширение", validate_database_paths(list(app_data_base = file.path(tempdir(), "x.txt")), allow_new = DATABASE_ALLOW_NEW)))
chk("пути: папка вместо файла отклоняется", grepl("указана папка", validate_database_paths(list(app_data_base = tempdir()))))
chk("пути: пустое значение, NA и не строка отклоняются", all(grepl("укажите", c(
  validate_database_paths(list(app_data_base = "")), validate_database_paths(list(app_data_base = "  ")),
  validate_database_paths(list(app_data_base = NA_character_)), validate_database_paths(list(app_data_base = 5))))))
chk("пути: в сообщении название базы, а не технический ключ", grepl("База приложения", validate_database_paths(list(app_data_base = ""))))
chk("пути: разрешён новый файл только для базы приложения", identical(DATABASE_ALLOW_NEW, "app_data_base"))

# ======================= создание базы и выбор из списка =======================
chk("полный путь: имя без папки относится к папке приложения", identical(full_database_path("app.db", "C:/x"), "C:/x/app.db"))
chk("полный путь: абсолютные пути (диск, UNC, корень) не меняются, слэши приводятся к /", identical(full_database_path("D:\\data\\a.db"), "D:/data/a.db") && identical(full_database_path("//srv/share/a.db"), "//srv/share/a.db") && identical(full_database_path("/tmp/a.db"), "/tmp/a.db"))
chk("полный путь: пробелы по краям убираются", identical(full_database_path("  C:/a.db  "), "C:/a.db"))
chk("SQLite: пустой файл, настоящая база; текст, папка и отсутствующий файл не подходят",
    is_sqlite_file(tmp_db) && is_sqlite_file(real_db) && !is_sqlite_file(text_file) && !is_sqlite_file(tempdir()) && !is_sqlite_file(new_path) && !is_sqlite_file(NULL))
chk("схема приложения: нет ни у пустого файла, ни у базы без action_log", !database_has_schema(tmp_db) && !database_has_schema(real_db))
chk("при запуске база создаётся, если файла нет или он пустой; база с таблицами и чужая база не пересоздаются",
    database_needs_creation(new_path) && database_needs_creation(tmp_db) && !database_needs_creation(real_db) && !database_needs_creation(text_file))

make_db <- tempfile(fileext = ".db")
created <- tryCatch(create_app_database(make_db), error = function(e) conditionMessage(e))
chk("create_app_database: база создана с таблицами приложения и справочником действий", identical(created, make_db) && database_has_schema(make_db) && {
  con <- dbConnect(SQLite(), make_db); on.exit(dbDisconnect(con), add = TRUE)
  all(c("action_log", "users", "actions", "auth_sessions", "login_attempts") %in% dbListTables(con)) && dbGetQuery(con, "SELECT COUNT(*) n FROM actions")$n > 0 &&
    dbGetQuery(con, "SELECT COUNT(*) n FROM users")$n == 0
})
chk("create_app_database: поверх существующего файла не создаёт", grepl("уже существует", tryCatch(create_app_database(make_db), error = function(e) conditionMessage(e))))
chk("create_app_database: неверное расширение и несуществующая папка отклоняются",
    !is.null(validate_new_database_path(file.path(tempdir(), "x.txt"))) && !is.null(validate_new_database_path(file.path(tempdir(), "no_dir", "x.db"))) && !is.null(validate_new_database_path("")))
chk("create_app_database: после неудачи недоделанный файл не остаётся", {
  bad <- tempfile(fileext = ".db"); res <- tryCatch(create_app_database(bad, script = "no_such_script.R"), error = function(e) "ошибка")
  identical(res, "ошибка") && !file.exists(bad)
})

dir_for_choices <- file.path(tempdir(), "choices_dir"); dir.create(dir_for_choices)
for (f in c("a.db", "b.sqlite", "c.txt", "d.SQLITE3")) file.create(file.path(dir_for_choices, f))
ch <- database_choices(dir_for_choices, current = file.path(dir_for_choices, "new_one.db"))
chk("выбор баз: берутся только файлы баз, текущий путь добавляется даже без файла", setequal(basename(unname(ch)), c("a.db", "b.sqlite", "d.SQLITE3", "new_one.db")))
chk("выбор баз: подписи содержат имя файла и размер, для отсутствующего файла пометка", all(grepl("a.db \\(", names(ch)[basename(ch) == "a.db"])) && grepl("будет создан", names(ch)[basename(ch) == "new_one.db"]))
chk("выбор баз: несуществующая папка и пустой список не ломают функцию", length(database_choices(file.path(tempdir(), "nope"))) == 0 && length(database_choices(character())) == 0)
chk("размер файла читается человеком", identical(format_file_size(500), "500 Б") && identical(format_file_size(2048), "2 КБ") && identical(format_file_size(3 * 1024^2), "3 МБ"))

# ======================= настройки безопасности =======================
sec_ok <- d$security
chk("безопасность: значения по умолчанию подходят", is.null(validate_security_settings(sec_ok)))
chk("безопасность: в конфиге по умолчанию есть все поля интерфейса", setequal(names(SECURITY_FIELDS), names(d$security)))
chk("безопасность: основные и дополнительные поля вместе дают все поля", setequal(c(SECURITY_MAIN, SECURITY_ADVANCED), names(SECURITY_FIELDS)) && !anyDuplicated(c(SECURITY_MAIN, SECURITY_ADVANCED)))
chk("безопасность: значения по умолчанию совпадают с настройками пакета", {
  s <- aa_settings(); all(vapply(names(SECURITY_FIELDS), function(k) identical(as.character(s[[k]]), as.character(d$security[[k]])), logical(1)))
})

bad <- function(key, value) { v <- sec_ok; v[[key]] <- value; validate_security_settings(v) }
chk("безопасность: ноль попыток отклоняется", grepl("от 1 до 100", bad("max_failed_logins", 0)))
chk("безопасность: слишком большое значение отклоняется", grepl("от 1 до 1440", bad("lockout_minutes", 100000)))
chk("безопасность: дробное число отклоняется", !is.null(bad("max_failed_logins", 2.5)))
chk("безопасность: NA и текст вместо числа отклоняются", !is.null(bad("max_failed_logins", NA)) && !is.null(bad("lockout_minutes", "много")))
chk("безопасность: число в виде текста из интерфейса принимается", is.null(bad("max_failed_logins", "7")))
chk("безопасность: неизвестный режим слабых паролей отклоняется", grepl("выберите", bad("password_policy", "strict")))
chk("безопасность: все три режима слабых паролей принимаются", all(vapply(c("off", "warn", "reject"), function(m) is.null(bad("password_policy", m)), logical(1))))
chk("безопасность: флаг должен быть да или нет", !is.null(bad("must_change_default", "yes")) && !is.null(bad("autologin", NA)) && is.null(bad("autologin", FALSE)))
chk("безопасность: минимальная длина пароля больше максимальной отклоняется", grepl("не может быть больше", bad("password_min", 64)) || { v <- sec_ok; v$password_min <- 20; v$password_max <- 10; grepl("не может быть больше", validate_security_settings(v)) })
chk("безопасность: неполный список (часть полей) проверяется по пакету", is.null(validate_security_settings(list(max_failed_logins = 3))))

chk("нормализация: числа становятся целыми, флаги и режимы остаются", identical(normalize_security_value("lockout_minutes", "15"), 15L) && identical(normalize_security_value("autologin", TRUE), TRUE) && identical(normalize_security_value("password_policy", "warn"), "warn"))
chk("нормализация: неверное значение даёт NULL", is.null(normalize_security_value("lockout_minutes", "x")) && is.null(normalize_security_value("autologin", "да")) && is.null(normalize_security_value("unknown", 1)) && is.null(normalize_security_value("lockout_minutes", c(1, 2))))

# ======================= описание изменений =======================
chk("изменения: только изменившиеся ключи", identical(describe_config_changes(list(a = 1, b = 2), list(a = 1, b = 3)), "b: 2 -> 3"))
chk("изменения: нет изменений - пустая строка", identical(describe_config_changes(list(a = TRUE), list(a = TRUE)), ""))
chk("изменения: несколько ключей через «;», роли через запятую", identical(describe_config_changes(list(a = c("x", "y"), b = FALSE), list(a = "x", b = TRUE)), "a: x,y -> x; b: FALSE -> TRUE"))
chk("изменения: новый ключ и отсутствующий ключ", identical(describe_config_changes(list(a = 1), list(a = 1, b = 2)), "b:  -> 2"))
chk("значения по умолчанию раздела берутся из default_config()", identical(default_section("logging"), d$logging))

finish_tests()
