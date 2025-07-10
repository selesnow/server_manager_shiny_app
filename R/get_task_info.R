#' Получить информацию о задаче из планировщика заданий
#'
#' @param task_name Путь и название задачи в планировщике заданий Windows
#'
#' @returns Данные по пзпдпче из планировщика
#' @export
get_task_info <- function(task_name) {
  
  tasks <- get_tasks() %>% 
    filter(TaskName == task_name) %>% 
    select(-matches('Repeat: ')) %>% 
    unique()
  
  if (nrow(tasks) == 0) {
    return('Задача не найдена')
  } 
  
  task_info <- tasks %>%
    slice(1) %>% 
    as.list() %>%
    imap_chr(~ glue("{.y}: {.x}")) %>%
    paste(collapse = "\n")
  
  return(task_info)
  
}