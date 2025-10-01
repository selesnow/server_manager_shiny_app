#' Отключение задач от мониторинга
#'
#' @param task_name Название задачи
#' @param until До какого времени отключить мониторинг
#'
#' @returns
#' @export
#'
#' @examples
forget_task <- function(
  task_name, 
  until
) {
  
  con <- dbConnect(SQLite(), conf$database_settings$task_log_base)
  
  df <- data.frame(task_name  = task_name,
                   quiet_till = until)
  
  dbWriteTable(con,
               name = 'forget_queue',
               value = df,
               append = TRUE)
  
  dbDisconnect(con)
  
}

# функция сообщит о том что с текущей ролью пользователя запуск задач недоступен
forget_task_na <- function(task_name) {
  return('Роль текущего пользователя чата не позволяет ему отключать задачи от мониторинга на сервере, пользователю необходимо связаться с администратором - [Alsey](https://t.me/AlexeySeleznev).')
}

forget_task_ls <- list(
  'enable' = forget_task,
  'disable' = forget_task_na
)