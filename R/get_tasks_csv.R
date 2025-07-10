#' Получает список задач из планировщика в виде CSV таблицы
#'
#' @returns
#' @export
#'
#' @examples
get_task_csv <- function() {
  readr::format_csv(get_tasks())
}