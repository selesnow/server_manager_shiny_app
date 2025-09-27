get_task_working_directory <- function(task_name) {
  cmd <- sprintf('schtasks /query /tn "%s" /xml', task_name)
  
  tryCatch({
    # Подавляем предупреждения о кодировке
    xml_output <- suppressWarnings(system(cmd, intern = TRUE))
    
    # Ищем строку с WorkingDirectory
    wd_line <- suppressWarnings(grep("<WorkingDirectory>", xml_output, value = TRUE))
    
    if (length(wd_line) == 0) {
      return(NA)
    }
    
    # Извлекаем путь
    working_dir <- gsub(".*<WorkingDirectory>([^<]*)</WorkingDirectory>.*", "\\1", wd_line[1])
    working_dir <- trimws(working_dir)
    
    if (working_dir == "") {
      return(NA)
    }
    
    return(working_dir)
    
  }, error = function(e) {
    warning(paste("Ошибка при получении данных задачи:", e$message))
    return(NULL)
  })
}
