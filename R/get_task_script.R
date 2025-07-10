#' Поиск листинга скрипта по названию задачи из планировщика
#'
#' @param task_name Путь и название задачи в планировщике заданий Windows
#'
#' @returns Листинг кода скрипта
#' @export
#'
get_task_script <- function(task_name) {
  
  tasks <- get_tasks() %>% 
    filter(TaskName == task_name) %>% 
    select(TaskName, `Start In`, `Task To Run`) %>% 
    unique()
  
  if (nrow(tasks) == 0) {
    return('Задача не найдена')
  } 
  
  task_log <- find_script(tasks$`Task To Run`, start_in = tasks$`Start In`)
  
  return(task_log)
  
}