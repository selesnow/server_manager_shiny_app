# Тесты конфигурации: значения по умолчанию, создание и дополнение config.yaml,
# согласованность прав доступа с кодом приложения.
# Запуск из корня проекта: Rscript tests/test_config.R

source("tests/helper.R")
source("create_config.R")

d <- default_config()
keys_default <- names(d$access_managemet)

# ======================= Права, которые использует код =======================
# Если код проверяет право, которого нет в значениях по умолчанию, то в новой
# установке это право будет ни у кого (так пропала кнопка «Код»).
files <- c("app.R", list.files("R", full.names = TRUE, pattern = "[.]R$"), list.files("modules", full.names = TRUE, pattern = "[.]R$"))
src <- unlist(lapply(files, readLines, warn = FALSE, encoding = "UTF-8"))
extract <- function(pattern, strip) gsub(strip, "", unlist(regmatches(src, gregexpr(pattern, src))))
used <- sort(unique(c(
  extract("access_managemet\\$(`[^`]+`|[A-Za-z_]+)", "^access_managemet\\$|`"),
  extract("user_can\\([^,]+,\\s*\"[^\"]+\"", "^.*,\\s*\"|\"$"),
  extract("require_permission\\([^,]+,[^,]+,\\s*\"[^\"]+\"", "^.*,\\s*\"|\"$"),
  extract("can\\(\"[^\"]+\"\\)", "^can\\(\"|\"\\)$")
)))
chk(paste0("все права, используемые кодом (", length(used), "), есть в значениях по умолчанию"), all(used %in% keys_default))
if (!all(used %in% keys_default)) cat("   отсутствуют в default_config():", paste(setdiff(used, keys_default), collapse = ", "), "\n")
chk("в значениях по умолчанию есть раздел security с лимитами входа", all(c("max_failed_logins", "lockout_minutes", "max_failed_logins_per_minute") %in% names(d$security)))
chk("CMD по умолчанию только для admin", identical(d$access_managemet$CMD, "admin"))

# ======================= ensure_config =======================
p <- temp_path("config_test.yaml"); unlink(p)
suppressMessages(ensure_config(p))
chk("нет файла: config.yaml создан со значениями по умолчанию", file.exists(p) && identical(yaml::read_yaml(p), d))

h1 <- tools::md5sum(p); m1 <- file.mtime(p); Sys.sleep(1.1)
suppressMessages(ensure_config(p))
chk("полный файл не перезаписывается", identical(unname(h1), unname(tools::md5sum(p))) && identical(m1, file.mtime(p)))

cur <- yaml::read_yaml(p)
cur$access_managemet$CMD <- c("admin", "user"); cur$access_managemet$`Службы` <- "admin"; cur$logging$cmd_log <- FALSE
cur$security$max_failed_logins <- 3; cur$database_settings$app_data_base <- "prod.db"
yaml::write_yaml(cur, p, fileEncoding = "UTF-8")
out <- suppressMessages(ensure_config(p))
chk("собственные значения (права, логирование, security, путь к БД) не перезаписываются",
    identical(out$access_managemet$CMD, c("admin", "user")) && identical(out$access_managemet$`Службы`, "admin") &&
      identical(out$logging$cmd_log, FALSE) && identical(out$security$max_failed_logins, 3) && identical(out$database_settings$app_data_base, "prod.db"))

# старый конфиг без новых разделов и прав (как у prod до обновления)
old <- cur; old$security <- NULL; old$access_managemet$`Просмотр кода` <- NULL; old$access_managemet$`AI анализ` <- NULL; old$extra_custom <- list(a = 1)
yaml::write_yaml(old, p, fileEncoding = "UTF-8")
out <- suppressMessages(ensure_config(p)); disk <- yaml::read_yaml(p)
chk("в старый конфиг дописан раздел security со значениями по умолчанию", identical(disk$security, d$security))
chk("в старый конфиг дописаны недостающие права", identical(disk$access_managemet$`Просмотр кода`, d$access_managemet$`Просмотр кода`) && identical(disk$access_managemet$`AI анализ`, d$access_managemet$`AI анализ`))
chk("при дописывании собственные значения и лишние ключи сохранены",
    identical(disk$access_managemet$CMD, c("admin", "user")) && identical(disk$access_managemet$`Службы`, "admin") &&
      identical(disk$database_settings$app_data_base, "prod.db") && identical(disk$extra_custom, list(a = 1)))

unlink(p)
finish_tests()
