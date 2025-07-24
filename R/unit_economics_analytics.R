#' Формирование отчёта по юнит экономике
#'
#' @param date_from Начальная дата в формате ГГГГ-ММ-ДД
#' @param date_to Конечная дата в формате ГГГГ-ММ-ДД
#'
#' @returns Таблицу задач с фактически заработанными деньгами
#' @export
#'
unit_economics_analytics <- function(date_from, date_to) {
  
  regular <- ue_regular(date_from, date_to)
  request <- ue_requested(date_from, date_to)
  
  ue <- bind_rows(regular, request)
  
  return(readr::format_csv(ue))
  
}
