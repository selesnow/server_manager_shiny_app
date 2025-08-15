#' Запрос данных по задаче
#'
#' @param link Ссылка на задачу в Планфикс
#'
#' @returns данные о задаче в JSON формате
#' @export
find_pf_task_data <- function(task_link) {
  
  rpup::pup_set_config_name('replica.cfg')
  rpup::pup_connection()
  pf_task_data <- rpup::pup_get_pf_task(
    '2011-09-08', date_to = Sys.Date(),
    general_ids = str_extract(task_link, '\\d+')
    )
  
  if (nrow(pf_task_data) > 0) {
    task_info <- pfworker::pfw_get_tasks(pf_task_data$pf_task_id, raw_result = TRUE)
    return(jsonlite::toJSON(task_info, pretty = T, auto_unbox = T))
  } else {
    return('Данные по указанной задаче не найдены!')
  }
  
}
