#' Поиск лога выполнения R скрипта по названию задачи из планировщика
#' @details
#' Используется только AI ассистентом для работы по логам задач
#' 
#'
#' @param task_name Путь и название задачи в планировщике заданий Windows
#'
#' @returns Текст лога выполнения скрипта
#' @export
#'
get_task_log <- function(task_name) {
  
  start_in    <- get_task_working_directory(task_name)
  task_to_tun <- get_task_command(task_name)

  task_log <- find_log(task_to_tun, start_in = start_in)
  
  return(task_log)
  
}