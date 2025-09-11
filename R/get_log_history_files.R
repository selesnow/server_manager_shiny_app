# ════════════════ ФУНКЦИЯ ДЛЯ РАБОТЫ С ИСТОРИЕЙ ЛОГОВ ════════════════

# Функция для получения списка файлов из папки rout_error_history
get_log_history_files <- function(start_in, task_to_run) {
  if (is.null(start_in) || is.na(start_in) || start_in == "") return(NULL)
  
  r_file    <- strsplit(unique(task_to_run), split = ' ')[[1]] %>%
    .[length(.)]
  rout_path <- str_glue('{r_file}out') %>% str_remove('^(R/|/R/|R\\\\)')
  
  # Очищаем путь от R папки если она есть
  start_in_clean <- str_remove(start_in, '\\\\R$|/R$|/R/$')
  history_folder <- file.path(start_in_clean, "rout_error_history")
  
  if (!dir.exists(history_folder)) return(NULL)
  
  # Ищем все .Rout файлы в папке
  log_files <- list.files(history_folder, 
                          pattern = str_glue("{rout_path}$"), 
                          full.names = TRUE, 
                          recursive = FALSE)
  
  if (length(log_files) == 0) return(NULL)
  
  # Сортируем по времени модификации (новые первыми)
  file_info <- file.info(log_files)
  log_files <- log_files[order(file_info$mtime, decreasing = TRUE)]
  
  # Создаем красивые названия для выпадающего списка
  choices <- setNames(
    log_files, 
    str_replace(
      basename(log_files),
      # 6 групп: YYYY MM DD HH MM SS + всё остальное
      "(\\d{4})_(\\d{2})_(\\d{2})_(\\d{2})_(\\d{2})_(\\d{2})_(.+)",
      "\\1-\\2-\\3 \\4:\\5:\\6 | \\7"
    )
    )
  
  new_files <- 
  
  return(choices)
}
