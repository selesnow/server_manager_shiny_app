get_task_command <- function(task_name) {
  # имя может прийти от AI ассистента, лишние аргументы в команду не пропускаем
  if (!is_safe_task_name(task_name)) return(NA)

  tryCatch({
    xml_output <- suppressWarnings(
      system2("schtasks", c("/query", "/tn", shQuote(task_name), "/xml"), stdout = TRUE, stderr = FALSE)
    )
    
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
