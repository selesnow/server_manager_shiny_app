# Одноразовая миграция паролей пользователей из открытого вида в хеши
#
# Запуск из корня проекта:
#   Rscript migrate_passwords.R                 - база из config.yaml
#   Rscript migrate_passwords.R путь/к/app.db   - конкретная база
#
# Перед изменениями рядом с базой создаётся резервная копия app.db.bak_<дата>.
# Скрипт можно запускать повторно, уже хешированные пароли пропускаются.
# Пароли в консоль не выводятся.

source("R/passwords.R")

args <- commandArgs(trailingOnly = TRUE)

db_path <- if (length(args) >= 1) {
  args[1]
} else {
  yaml::read_yaml("config.yaml")$database_settings$app_data_base
}

cat("База данных:", normalizePath(db_path, mustWork = FALSE), "\n")

res <- migrate_passwords_to_hash(db_path)

cat("Резервная копия:", res$backup, "\n")
cat("Захешировано паролей:", length(res$migrated), "\n")
if (length(res$migrated) > 0) cat("  ", paste(res$migrated, collapse = ", "), "\n")
cat("Уже были хешами (пропущено):", length(res$skipped), "\n")
if (length(res$skipped) > 0) cat("  ", paste(res$skipped, collapse = ", "), "\n")
