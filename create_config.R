library(yaml)

cat('Создание конфигурационного файла.')

# Создаём список с настройками
config <- list(
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
  access_managemet = list(
    "Задачи" = c("admin", "user", "viewer"),
    "Службы" = c("admin", "user"),
    "AI Ассистент" = c("admin", "user", "viewer"),
    "Поиск по файлам" = c("admin", "user"),
    "Процессы" = c("admin", "user"),
    "CMD" = c("admin", "user"),
    "Доступ" = "admin",
    "Статистика" = c("admin", "user", "viewer"),
    "Логи" = "admin",
    "Readme" = c("admin", "user", "viewer"),
    "News" = c("admin", "user", "viewer"),
    "Запуск задач" = c("admin", "user"),
    "Активация задач" = c("admin", "user"),
    "Управление службами" = c("admin", "user"),
    "AI анализ" = c("admin", "user"),
    "Управление мониторингом задач" = c("admin", "user")
  )
)

# Сохраняем в YAML
yaml::write_yaml(config, "config.yaml", fileEncoding = "UTF-8")

cat("Файл config.yaml создан в директории:", getwd(), "\n")
