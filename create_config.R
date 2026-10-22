library(yaml)

# Конфигурационный файл config.yaml у каждой копии приложения (dev и prod) свой
# и в git не хранится, при деплое он не перезаписывается. Если файла нет, он
# создаётся из значений по умолчанию, если есть, в него только добавляются
# недостающие настройки (например, новые права доступа из свежей версии
# приложения), существующие значения не меняются.

#' Настройки по умолчанию
default_config <- function() {
  list(
    database_settings = list(
      app_data_base = "app.db",
      task_log_base = "C:/scripts/alsey/netpeak_core/nc_analytics_team/telegram_bot/bot_db.db"
    ),
    logging = list(
      session_log       = TRUE,
      action_log        = TRUE,
      error_log         = TRUE,
      ai_chat_log       = TRUE,
      find_in_files_log = TRUE,
      cmd_log           = TRUE
    ),
    # настройки авторизации (пакет appauth, см. ?aa_settings); незаданные берутся из пакета
    security = list(
      max_failed_logins            = 5,   # неудачных попыток до блокировки логина
      lockout_minutes              = 15,  # окно подсчёта и длительность блокировки, минут
      max_failed_logins_per_minute = 30,  # общий лимит неудачных попыток по всем логинам за минуту
      must_change_default          = TRUE # пароль, который задал администратор, по умолчанию временный (смена при входе)
    ),
    access_managemet = list(
      "Задачи" = c("admin", "user", "viewer"),
      "Службы" = c("admin", "user"),
      "AI Ассистент" = c("admin", "user", "viewer"),
      "Поиск по файлам" = c("admin", "user"),
      "Процессы" = c("admin", "user"),
      "CMD" = "admin",
      "Доступ" = "admin",
      "Статистика" = c("admin", "user", "viewer"),
      "Логи" = "admin",
      "Readme" = c("admin", "user", "viewer"),
      "News" = c("admin", "user", "viewer"),
      "Запуск задач" = c("admin", "user"),
      "Активация задач" = c("admin", "user"),
      "Управление службами" = c("admin", "user"),
      "AI анализ" = c("admin", "user"),
      "Просмотр кода" = c("admin", "user"),
      "Управление мониторингом задач" = c("admin", "user")
    )
  )
}

#' Дописывает в current настройки из defaults, которых там нет
#'
#' Существующие значения не изменяются, лишние ключи в current сохраняются.
merge_missing_config <- function(current, defaults) {
  for (key in names(defaults)) {
    if (is.null(current[[key]])) {
      current[[key]] <- defaults[[key]]
    } else if (is.list(current[[key]]) && !is.null(names(current[[key]])) &&
               is.list(defaults[[key]]) && !is.null(names(defaults[[key]]))) {
      current[[key]] <- merge_missing_config(current[[key]], defaults[[key]])
    }
  }
  current
}

#' Создаёт config.yaml или дополняет существующий недостающими настройками
#'
#' @param path путь к config.yaml
#'
#' @returns список с настройками
ensure_config <- function(path = "config.yaml") {
  defaults <- default_config()

  if (!file.exists(path)) {
    yaml::write_yaml(defaults, path, fileEncoding = "UTF-8")
    message("Создан файл ", path, " с настройками по умолчанию: ", normalizePath(path))
    return(invisible(defaults))
  }

  current <- yaml::read_yaml(path)
  merged  <- merge_missing_config(current, defaults)

  # файл перезаписываем только если в него действительно что-то добавили
  if (!identical(merged, current)) {
    yaml::write_yaml(merged, path, fileEncoding = "UTF-8")
    message("В ", path, " добавлены недостающие настройки по умолчанию")
  }

  invisible(merged)
}
