#' Поиск листинга скрипта по названию задачи из планировщика
#'
#' @param task_name Путь и название задачи в планировщике заданий Windows
#'
#' @returns Листинг кода скрипта
#' @export
#'
get_task_script <- function(task_name) {
  
  start_in    <- get_task_working_directory(task_name)
  task_to_tun <- get_task_command(task_name)
  
  task_script <- find_script(task_to_tun, start_in = start_in)
  
  return(task_script)
  
}