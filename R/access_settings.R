# Настройки приложения на вкладке «Доступ»: описание настроек для интерфейса,
# проверка значений и описание изменений для журнала. Всё здесь чистые функции без
# Shiny, чтобы их можно было проверять автотестами (tests/test_access_settings.R).

# --- права ролей ----------------------------------------------------------------------

# группы прав для матрицы: названия прав должны совпадать с ключами access_managemet
PERMISSION_GROUPS <- list(
  "Вкладки приложения" = c(
    "Задачи", "Службы", "AI Ассистент", "Поиск по файлам", "Процессы",
    "CMD", "Доступ", "Статистика", "Логи", "Readme", "News"
  ),
  "Действия внутри вкладок" = c(
    "Запуск задач", "Активация задач", "Управление службами", "AI анализ",
    "Просмотр кода", "Управление мониторингом задач"
  )
)

PERMISSION_HINTS <- c(
  "Задачи"            = "Список задач планировщика, логи и статусы",
  "Службы"            = "Службы Analytics на сервере",
  "AI Ассистент"      = "Чат с AI для работы со скриптами",
  "Поиск по файлам"   = "Поиск по скриптам на сервере",
  "Процессы"          = "Список процессов сервера",
  "CMD"               = "Командная строка сервера",
  "Доступ"            = "Эта вкладка: пользователи и настройки",
  "Статистика"        = "Сводная статистика по задачам",
  "Логи"              = "Журналы использования приложения",
  "Readme"            = "Информационная вкладка README",
  "News"              = "Информационная вкладка NEWS",
  "Запуск задач"      = "Кнопка запуска задачи",
  "Активация задач"   = "Включение и отключение задач",
  "Управление службами" = "Запуск, остановка и перезапуск служб",
  "AI анализ"         = "Анализ логов и кода с помощью AI",
  "Просмотр кода"     = "Кнопка «Код» в карточке задачи",
  "Управление мониторингом задач" = "Отключение задач от мониторинга"
)

#' Раскладка прав по группам для матрицы
#'
#' @param perms названия прав (имена access_managemet из конфига)
#'
#' @returns именованный список групп; права вне известных групп попадают в «Прочее»
permission_layout <- function(perms) {
  groups <- lapply(PERMISSION_GROUPS, function(g) g[g %in% perms])
  groups <- groups[lengths(groups) > 0]

  rest <- setdiff(perms, unlist(PERMISSION_GROUPS))
  if (length(rest) > 0) groups[["Прочее"]] <- rest

  groups
}

#' Идентификатор переключателя в матрице прав
#'
#' Названия прав на русском и с пробелами, поэтому в идентификаторах используются
#' номера: право по порядку в конфиге и роль по порядку в настройках.
matrix_input_id <- function(perm_index, role_index) {
  paste0("perm_", perm_index, "_", role_index)
}

#' Роли права из состояния переключателей матрицы
#'
#' Роли, которых нет в настройках пакета (устаревшие значения в конфиге), не теряются.
#' У права «Доступ» роль администратора остаётся всегда: иначе можно закрыть себе
#' вкладку настроек и потерять управление.
#'
#' @param checked логический вектор по ролям (в порядке roles), NA и NULL считаются «выключено»
#' @param roles допустимые роли из настроек
#' @param old_roles роли права в конфиге до изменения
#' @param perm название права
#' @param admin_role роль администратора
roles_from_matrix <- function(checked, roles, old_roles, perm, admin_role) {
  checked <- isTRUE_vec(checked, length(roles))
  new_roles <- c(roles[checked], setdiff(old_roles, roles))

  if (identical(perm, "Доступ") && !admin_role %in% new_roles) {
    new_roles <- c(admin_role, new_roles)
  }

  unique(new_roles)
}

isTRUE_vec <- function(x, n) {
  out <- logical(n)
  if (length(x) > 0) out[seq_len(min(n, length(x)))] <- vapply(x[seq_len(min(n, length(x)))], isTRUE, logical(1))
  out
}

# --- журналы ----------------------------------------------------------------------------

LOGGING_META <- list(
  session_log       = c("Журнал сессий", "Когда пользователь вошёл в приложение и вышел"),
  action_log        = c("Журнал действий", "Кто и какое действие выполнил: запуск задач, изменение прав, работа с пользователями"),
  error_log         = c("Журнал ошибок", "Ошибки, которые возникли в приложении"),
  ai_chat_log       = c("Журнал AI-чата", "Сообщения пользователей и ответы ассистента"),
  find_in_files_log = c("Журнал поиска по файлам", "Поисковые запросы по скриптам сервера"),
  cmd_log           = c("Журнал командной строки", "Введённые команды и их результат (пароли в командах не скрываются)")
)

#' Название и подсказка переключателя журнала
#'
#' @returns вектор из двух строк: название и подсказка; для неизвестного ключа название
#'   совпадает с ключом, подсказка пустая
logging_meta <- function(key) {
  if (key %in% names(LOGGING_META)) LOGGING_META[[key]] else c(key, "")
}

# --- базы данных ------------------------------------------------------------------------

DATABASE_META <- list(
  app_data_base = c("База приложения", "Пользователи, сессии и журналы приложения"),
  task_log_base = c("База мониторинга задач", "База Telegram-бота: задачи, отключённые от мониторинга")
)

database_meta <- function(key) {
  if (key %in% names(DATABASE_META)) DATABASE_META[[key]] else c(key, "")
}

# базы, для которых можно указать ещё не созданный файл: при запуске приложение создаст
# его с нуля (create_db.R). База бота принадлежит другому сервису, её создаём не мы.
DATABASE_ALLOW_NEW <- "app_data_base"

DATABASE_FILE_PATTERN <- "[.](db|sqlite|sqlite3)$"

#' Это файл базы SQLite (или пустой файл, который SQLite считает пустой базой)?
is_sqlite_file <- function(path) {
  if (!(is.character(path) && length(path) == 1 && !is.na(path) && file.exists(path) && !dir.exists(path))) {
    return(FALSE)
  }

  if (file.size(path) == 0) return(TRUE)

  header <- tryCatch({
    con <- file(path, "rb")
    on.exit(close(con))
    readBin(con, "raw", 15)
  }, error = function(e) raw(0))

  identical(header, charToRaw("SQLite format 3"))
}

#' Есть ли в базе таблицы приложения (создана ли она скриптом create_db.R)
database_has_schema <- function(path) {
  if (!is_sqlite_file(path)) return(FALSE)

  con <- tryCatch(DBI::dbConnect(RSQLite::SQLite(), path), error = function(e) NULL)
  if (is.null(con)) return(FALSE)
  on.exit(DBI::dbDisconnect(con))

  "action_log" %in% DBI::dbListTables(con)
}

#' Нужно ли создавать базу с нуля при запуске приложения
#'
#' Нужно, если файла нет или это пустая база SQLite (вообще без таблиц). База, в которой уже
#' есть таблицы, не пересоздаётся: create_db.R не рассчитан на повторный запуск, а чужую
#' базу с другими таблицами приложение не должно дополнять само.
database_needs_creation <- function(path) {
  if (!file.exists(path)) return(TRUE)
  if (!is_sqlite_file(path)) return(FALSE)

  con <- tryCatch(DBI::dbConnect(RSQLite::SQLite(), path), error = function(e) NULL)
  if (is.null(con)) return(FALSE)
  on.exit(DBI::dbDisconnect(con))

  length(DBI::dbListTables(con)) == 0
}

#' Проверка пути к новой базе перед созданием
#'
#' @returns `NULL`, если базу можно создать, иначе текст проблемы
validate_new_database_path <- function(path) {
  if (!(is.character(path) && length(path) == 1 && !is.na(path) && nzchar(trimws(path)))) {
    return("Укажите путь к новому файлу базы")
  }
  if (!grepl(DATABASE_FILE_PATTERN, path, ignore.case = TRUE)) {
    return("Файл базы должен иметь расширение .db, .sqlite или .sqlite3")
  }
  if (file.exists(path) || dir.exists(path)) {
    return(paste0("Файл уже существует (", path, "): новую базу поверх него не создаём"))
  }
  if (!dir.exists(dirname(path))) {
    return(paste0("Папка не найдена (", dirname(path), ")"))
  }
  if (file.access(dirname(path), 2) != 0) {
    return(paste0("Нет прав на запись в папку (", dirname(path), ")"))
  }

  NULL
}

#' Приводит введённый путь к полному: имя без папки относится к папке приложения
full_database_path <- function(path, base_dir = getwd()) {
  path <- trimws(path)
  if (!nzchar(path)) return(path)

  is_absolute <- grepl("^([A-Za-z]:[/\\\\]|[/\\\\]{2}|/)", path)
  full <- if (is_absolute) path else file.path(base_dir, path)
  gsub("\\\\", "/", full)
}

#' Проверка путей к базам данных
#'
#' Существующий файл должен быть базой SQLite. Для баз из `allow_new` разрешён и ещё не
#' созданный файл (при запуске приложение создаст его с нуля), но папка должна существовать.
#' Остальные базы должны существовать: иначе SQLite молча создаст пустой файл без таблиц.
#'
#' @param values именованный список путей
#' @param allow_new ключи баз, для которых можно указать новый файл
#'
#' @returns `NULL`, если всё в порядке, иначе текст первой проблемы
validate_database_paths <- function(values, allow_new = character()) {
  for (key in names(values)) {
    path <- values[[key]]
    label <- database_meta(key)[1]

    if (!(is.character(path) && length(path) == 1 && !is.na(path) && nzchar(trimws(path)))) {
      return(paste0("«", label, "»: укажите путь к файлу базы"))
    }

    if (dir.exists(path)) {
      return(paste0("«", label, "»: указана папка, а нужен файл базы (", path, ")"))
    }

    if (file.exists(path)) {
      if (!is_sqlite_file(path)) {
        return(paste0("«", label, "»: это не база SQLite (", path, ")"))
      }
      next
    }

    if (!key %in% allow_new) {
      return(paste0("«", label, "»: файл не найден (", path, ")"))
    }

    problem <- validate_new_database_path(path)
    if (!is.null(problem)) return(paste0("«", label, "»: ", problem))
  }

  NULL
}

#' Файлы баз в папках для списка выбора
#'
#' @param dirs папки для просмотра (без вложенных)
#' @param current пути, которые должны быть в списке, даже если файла нет
#'
#' @returns именованный вектор: имя это подпись «файл (размер, дата)», значение это полный путь
database_choices <- function(dirs, current = character()) {
  files <- unlist(lapply(unique(dirs[dir.exists(dirs)]), function(d) {
    list.files(d, pattern = DATABASE_FILE_PATTERN, full.names = TRUE, ignore.case = TRUE)
  }))
  files <- sort(unique(gsub("\\\\", "/", c(files, current[nzchar(current)]))))
  files <- utils::head(files, 200)

  label <- vapply(files, function(f) {
    if (file.exists(f)) {
      info <- file.info(f)
      sprintf("%s (%s, %s)", basename(f), format_file_size(info$size), format(info$mtime, "%Y-%m-%d"))
    } else {
      paste0(basename(f), " (файла нет, будет создан)")
    }
  }, character(1))

  stats::setNames(files, label)
}

format_file_size <- function(bytes) {
  if (is.na(bytes)) return("?")
  if (bytes < 1024) return(paste0(bytes, " Б"))
  if (bytes < 1024^2) return(paste0(round(bytes / 1024), " КБ"))
  paste0(round(bytes / 1024^2, 1), " МБ")
}

#' Создаёт пустую базу приложения с нуля скриптом create_db.R
#'
#' Работает с отдельной средой, поэтому рабочий конфиг и подключения приложения не затрагиваются.
#' Если создание не удалось, недоделанный файл удаляется.
#'
#' @param path полный путь к новому файлу
#' @param script путь к create_db.R
#'
#' @returns путь к созданной базе (невидимо) либо ошибка с понятным текстом
create_app_database <- function(path, script = "create_db.R") {
  problem <- validate_new_database_path(path)
  if (!is.null(problem)) stop(problem, call. = FALSE)

  env <- new.env(parent = globalenv())
  env$conf <- list(database_settings = list(app_data_base = path))

  ok <- tryCatch({
    utils::capture.output(source(script, local = env, encoding = "UTF-8"))
    database_has_schema(path)
  }, error = function(e) {
    message("Не удалось создать базу: ", conditionMessage(e))
    FALSE
  })

  if (!ok) {
    unlink(path)
    stop("Не удалось создать базу данных, подробности в консоли приложения", call. = FALSE)
  }

  invisible(path)
}

# --- безопасность входа -----------------------------------------------------------------

# type: int (целое число в пределах min..max), flag (да или нет), enum (choices)
SECURITY_FIELDS <- list(
  max_failed_logins = list(
    label = "Неудачных попыток до блокировки", type = "int", min = 1, max = 100, unit = "попыток",
    hint = "Столько ошибок подряд закрывают вход под этим логином"
  ),
  lockout_minutes = list(
    label = "Срок блокировки входа", type = "int", min = 1, max = 1440, unit = "минут",
    hint = "Окно подсчёта ошибок и время, на которое закрывается вход"
  ),
  max_failed_logins_per_minute = list(
    label = "Общий лимит ошибок в минуту", type = "int", min = 1, max = 1000, unit = "в минуту",
    hint = "Если за минуту накопилось столько ошибок по всем логинам, вход временно закрыт для всех"
  ),
  password_policy = list(
    label = "Слабые пароли", type = "enum",
    choices = c("Предупреждать, но принять" = "warn", "Отклонять" = "reject", "Не проверять" = "off"),
    hint = "Простые пароли (из списка распространённых, совпадающие с логином, из одного символа)"
  ),
  must_change_default = list(
    label = "Пароль от администратора временный", type = "flag",
    hint = "Пользователь обязан сменить заданный администратором пароль при первом входе"
  ),
  password_min = list(
    label = "Минимальная длина пароля", type = "int", min = 1, max = 64, unit = "символов",
    hint = "Действует только для новых и сбрасываемых паролей"
  ),
  password_max = list(
    label = "Максимальная длина пароля", type = "int", min = 8, max = 1024, unit = "символов",
    hint = "Ограничение сверху для новых паролей"
  ),
  autologin = list(
    label = "Автовход по cookie", type = "flag",
    hint = "Если выключить, пароль придётся вводить при каждой загрузке страницы"
  ),
  session_days = list(
    label = "Срок автовхода", type = "int", min = 1, max = 365, unit = "дней",
    hint = "Через сколько дней токен автовхода перестаёт действовать"
  ),
  session_check_seconds = list(
    label = "Проверка открытой сессии", type = "int", min = 5, max = 3600, unit = "секунд",
    hint = "Как часто проверять, что пользователя не заблокировали, не удалили и не сменили ему роль"
  ),
  attempts_retention_hours = list(
    label = "Хранение попыток входа", type = "int", min = 1, max = 720, unit = "часов",
    hint = "Сколько хранить записи о неудачных и успешных попытках"
  ),
  token_retention_days = list(
    label = "Хранение просроченных токенов", type = "int", min = 1, max = 365, unit = "дней",
    hint = "Сколько хранить просроченные и отозванные токены автовхода"
  )
)

SECURITY_MAIN <- c(
  "max_failed_logins", "lockout_minutes", "max_failed_logins_per_minute",
  "password_policy", "must_change_default"
)
SECURITY_ADVANCED <- setdiff(names(SECURITY_FIELDS), SECURITY_MAIN)

#' Приведение значения поля безопасности к нужному типу
#'
#' @returns значение нужного типа либо `NULL`, если привести нельзя
normalize_security_value <- function(key, value) {
  meta <- SECURITY_FIELDS[[key]]
  if (is.null(meta) || is.null(value) || length(value) != 1) return(NULL)

  switch(
    meta$type,
    int = {
      v <- suppressWarnings(as.numeric(value))
      if (is.na(v) || !is.finite(v) || v != round(v)) NULL else as.integer(v)
    },
    flag = if (is.logical(value) && !is.na(value)) value else NULL,
    enum = if (is.character(value) && value %in% meta$choices) value else NULL
  )
}

#' Проверка настроек безопасности перед записью в конфиг
#'
#' Сначала проверяются пределы каждого поля, потом согласованность (минимальная длина
#' пароля не больше максимальной) и, наконец, настройки собираются пакетом appauth.
#'
#' @param values именованный список значений из интерфейса или конфига
#'
#' @returns `NULL`, если значения подходят, иначе текст первой проблемы
validate_security_settings <- function(values) {
  for (key in names(values)) {
    meta <- SECURITY_FIELDS[[key]]
    if (is.null(meta)) next

    v <- normalize_security_value(key, values[[key]])
    if (is.null(v)) {
      return(switch(
        meta$type,
        int  = paste0("«", meta$label, "»: введите целое число от ", meta$min, " до ", meta$max),
        flag = paste0("«", meta$label, "»: значение должно быть «да» или «нет»"),
        enum = paste0("«", meta$label, "»: выберите значение из списка")
      ))
    }

    if (meta$type == "int" && (v < meta$min || v > meta$max)) {
      return(paste0("«", meta$label, "»: значение должно быть от ", meta$min, " до ", meta$max))
    }
  }

  pmin_v <- normalize_security_value("password_min", values$password_min)
  pmax_v <- normalize_security_value("password_max", values$password_max)
  if (!is.null(pmin_v) && !is.null(pmax_v) && pmin_v > pmax_v) {
    return("Минимальная длина пароля не может быть больше максимальной")
  }

  # пакет получает уже приведённые к типам значения (число из текстового поля - целое)
  normalized <- Filter(Negate(is.null), stats::setNames(
    lapply(names(values), function(k) if (k %in% names(SECURITY_FIELDS)) normalize_security_value(k, values[[k]]) else values[[k]]),
    names(values)
  ))

  tryCatch(
    {
      appauth::aa_settings_from_list(normalized)
      NULL
    },
    error = function(e) conditionMessage(e)
  )
}

# --- изменения для журнала ---------------------------------------------------------------

#' Краткое описание изменений между двумя списками настроек
#'
#' Для значения в журнале действий: `ключ: было -> стало`, только изменившиеся ключи.
#' Настройки не содержат секретов, поэтому значения выводятся как есть.
#'
#' @param old,new именованные списки одинаковой структуры
#'
#' @returns строка (пустая, если изменений нет)
describe_config_changes <- function(old, new) {
  fmt <- function(x) paste(as.character(unlist(x)), collapse = ",")

  keys <- union(names(old), names(new))
  changes <- vapply(keys, function(k) {
    a <- fmt(old[[k]]); b <- fmt(new[[k]])
    if (identical(a, b)) "" else paste0(k, ": ", a, " -> ", b)
  }, character(1))

  paste(changes[nzchar(changes)], collapse = "; ")
}

#' Значения раздела по умолчанию из default_config()
default_section <- function(section) {
  default_config()[[section]]
}
