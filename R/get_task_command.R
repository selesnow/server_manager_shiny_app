get_task_command <- function(task_name) {
  cmd <- sprintf('schtasks /query /tn "%s" /xml', task_name)
  
  tryCatch({
    xml_output <- suppressWarnings(system(cmd, intern = TRUE))
    
    # Ищем строку с Arguments
    args_line <- suppressWarnings(grep("<Arguments>", xml_output, value = TRUE))
    
    if (length(args_line) == 0) {
      return(NA)
    }
    
    # Извлекаем аргументы
    arguments <- gsub(".*<Arguments>([^<]*)</Arguments>.*", "\\1", args_line[1])
    arguments <- trimws(arguments)
    
    if (arguments == "") {
      return(NA)
    }
    
    return(arguments)
    
  }, error = function(e) {
    warning(paste("Ошибка при получении аргументов задачи:", e$message))
    return(NULL)
  })
}
