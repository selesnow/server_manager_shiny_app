#' Запуск задачи в планировщике заданий Windows на сервере аналитики
#'
#' @param task_name Путь и название задачи в планировщике заданий Windows
#'
#' @returns Информацию о том какая задача запущена
#' @export
run_server_task <- function(task_name) {
  # имя задачи может прийти от AI ассистента, запускаем только существующие задачи
  if (!task_exists_in_scheduler(task_name)) {
    return('Задача не найдена в планировщике заданий, запуск отменён.')
  }

  taskscheduler_runnow(taskname = task_name)
  cli::cli_alert_info('Задача {task_name} запущена на сервере')
  return(glue::glue('Задача {task_name} запущена на сервере'))
}

# функция сообщит о том что с текущей ролью пользователя запуск задач недоступен
run_server_task_na <- function(task_name) {
  return('Роль текущего пользователя чата не позволяет ему запускать задачи на сервере, пользователю необходимо связаться с администратором - [Alsey](https://t.me/AlexeySeleznev).')
}

run_server_task_ls <- list(
  'enable' = run_server_task,
  'disable' = run_server_task_na
)