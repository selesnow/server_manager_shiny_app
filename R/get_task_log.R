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
  
  tasks <- get_tasks() %>% 
    filter(TaskName == task_name) %>% 
    select(TaskName, `Start In`, `Task To Run`) %>% 
    unique()
  
  if (nrow(tasks) == 0) {
    return('Задача не найдена')
  } 
  
  task_log <- find_log(tasks$`Task To Run`, start_in = tasks$`Start In`)
  
  return(task_log)
  
}