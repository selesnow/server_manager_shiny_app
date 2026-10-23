# Резервные копии config.yaml: перед каждым изменением настроек предыдущий конфиг
# складывается в папку config_backups (последние 10), из неё же его можно вернуть.
# Функции не зависят от Shiny и работают с путями из аргументов, поэтому проверяются
# автотестами (tests/test_config_backups.R).

CONFIG_BACKUP_DIR  <- "config_backups"
CONFIG_BACKUP_KEEP <- 10
CONFIG_BACKUP_PATTERN <- "^config_[0-9]{8}_[0-9]{6}(_[0-9]+)?[.]yaml$"

#' Сохранить копию конфига перед изменением
#'
#' Если содержимое совпадает с последней копией, новая не создаётся (в папке не копятся
#' одинаковые файлы). Лишние старые копии удаляются, остаются `keep` последних.
#'
#' @param config_file путь к config.yaml
#' @param dir папка для копий
#' @param keep сколько копий хранить
#' @param now время для имени файла
#'
#' @returns путь к созданной копии, `NULL` если копия не понадобилась (невидимо)
backup_config <- function(config_file = "config.yaml", dir = CONFIG_BACKUP_DIR, keep = CONFIG_BACKUP_KEEP, now = Sys.time()) {
  if (!file.exists(config_file)) return(invisible(NULL))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  existing <- list_config_backups(dir)
  if (nrow(existing) > 0 &&
      identical(unname(tools::md5sum(config_file)), unname(tools::md5sum(existing$path[1])))) {
    return(invisible(NULL))
  }

  stamp <- format(now, "%Y%m%d_%H%M%S")
  target <- file.path(dir, paste0("config_", stamp, ".yaml"))
  n <- 1
  while (file.exists(target)) {
    n <- n + 1
    target <- file.path(dir, paste0("config_", stamp, "_", n, ".yaml"))
  }

  file.copy(config_file, target)

  old <- list_config_backups(dir)
  if (nrow(old) > keep) unlink(old$path[(keep + 1):nrow(old)])

  invisible(target)
}

#' Список копий конфига, новые первыми
#'
#' @returns таблица с колонками `file` (имя), `path`, `time` (время изменения), `size` (байт)
list_config_backups <- function(dir = CONFIG_BACKUP_DIR) {
  files <- if (dir.exists(dir)) list.files(dir, pattern = CONFIG_BACKUP_PATTERN) else character()
  files <- sort(files, decreasing = TRUE)   # в имени дата, поэтому по имени это и порядок по времени

  info <- file.info(file.path(dir, files))
  data.frame(
    file = files, path = file.path(dir, files), time = info$mtime, size = info$size,
    stringsAsFactors = FALSE
  )
}

#' Подпись копии конфига для списка выбора
config_backup_label <- function(time, size) {
  sprintf("%s (%s)", format(time, "%Y-%m-%d %H:%M:%S"), format_file_size(size))
}

#' Проверка конфига из копии перед восстановлением
#'
#' @param cfg список из `yaml::read_yaml()`
#' @param admin_role роль администратора
#'
#' @returns `NULL`, если конфиг подходит, иначе текст проблемы
validate_restored_config <- function(cfg, admin_role = "admin") {
  required <- c("database_settings", "logging", "security", "access_managemet")
  if (!is.list(cfg) || !all(required %in% names(cfg))) {
    return(paste0("В копии нет нужных разделов (", paste(required, collapse = ", "), ")"))
  }

  matrix_ok <- all(vapply(cfg$access_managemet, function(r) is.character(r), logical(1)))
  if (!matrix_ok || length(cfg$access_managemet) == 0) {
    return("В копии некорректный раздел прав ролей")
  }

  problem <- validate_security_settings(cfg$security)
  if (!is.null(problem)) return(paste("В копии неверные настройки входа:", problem))

  NULL
}

#' Читает копию конфига по имени файла из списка (имя от клиента не доверяется)
#'
#' Принимается только имя вида config_ГГГГММДД_ЧЧММСС.yaml из папки копий: пути и `..` отклоняются.
#'
#' @returns список конфига либо `NULL`, если имя недопустимо или файла нет
read_config_backup <- function(file, dir = CONFIG_BACKUP_DIR) {
  if (!(is.character(file) && length(file) == 1 && !is.na(file) && grepl(CONFIG_BACKUP_PATTERN, file))) {
    return(NULL)
  }

  path <- file.path(dir, file)
  if (!file.exists(path)) return(NULL)

  tryCatch(yaml::read_yaml(path), error = function(e) NULL)
}
