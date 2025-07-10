#' Получить список задач из планировщика Windows работа которых завершилась ошибкой
#'
#' @returns Информацию по названию задачи и код ошибки
#' @export
#'
get_failed_tasks <- function() {
  
  tasks <- get_tasks() %>% 
    filter(`Scheduled Task State` == "Enabled") %>% 
    filter(! `Last Result` %in% c("Успешно (0)", "Ещё не запускалась (267011)", "Задача выполняется (267009)")) %>% 
    select(TaskName, `Last Result`) %>% 
    unique()
  
  if (nrow(tasks) == 0) {
    return('Все задачи выполнены успешно!')
  } 
  
  task_info <- pmap_chr(tasks, ~ glue("{..1}: `{..2}`")) %>%
    paste(collapse = "\n")
  
  return(task_info)
  
}
