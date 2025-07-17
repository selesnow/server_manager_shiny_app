#' Изменить статус задачи в планировщике заданий Windows
#'
#' @param task_name название задачи
#' @param action что необходимо стедать активировать или отключить задачу, допустимые значения Enable и Disable
#'
#' @returns информацию о том удалось ли изменить статус задачи
#' @export
task_state_change <- function(task_name, action = c("Enable", "Disable")) {
  # Проверка аргументов
  action <- match.arg(action)
  
  # Подготовка аргументов команды
  args <- c("/Change", "/TN", shQuote(task_name), paste0("/", action))
  
  # Выполнение команды
  result <- system2("schtasks", args = args, stdout = TRUE, stderr = TRUE)
  
  # Проверка успешности
  if (any(grepl("SUCCESS", result, ignore.case = TRUE))) {
    message(sprintf("Задача '%s' успешно %s.", task_name, 
                    if (action == "Enable") "включена" else "отключена"))
    invisible(TRUE)
  } else {
    warning(sprintf("Не удалось %s задачу '%s':\n%s",
                    tolower(action), task_name, paste(result, collapse = "\n")))
    invisible(FALSE)
  }
}
