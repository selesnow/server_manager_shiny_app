#' Запуск задачи в планировщике заданий Windows на сервере аналитики
#'
#' @param task_name Путь и название задачи в планировщике заданий Windows
#'
#' @returns Информацию о том какая задача запущена
#' @export
run_server_task <- function(task_name) {
  taskscheduler_runnow(taskname = task_name)
  cli::cli_alert_info('Задача {task_name} запущена на сервере')
  return(glue::glue('Задача {task_name} запущена на сервере'))
}
