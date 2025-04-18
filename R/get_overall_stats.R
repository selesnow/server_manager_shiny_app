#' Функция для получения общей статистики по задачам
#'
#' @param tasks Таблица с информацией по задачам
#'
#' @returns список с расчитанной статистикой
#' @export
#'
get_overall_stats <- function(tasks) {
  tasks <- tasks %>%
    select(TaskName, Client, Author, `New Structure`) %>% 
    unique()
  
  total_crons <- length(unique(tasks$TaskName))
  new_structure_crons <- length(unique(filter(tasks, `New Structure`)$TaskName))
  crons_to_move <- total_crons - new_structure_crons
  
  new_structure_percent <- round(new_structure_crons / total_crons * 100, 0)
  to_move_percent <- round(crons_to_move / total_crons * 100, 0)
  
  list(
    total_crons = total_crons,
    new_structure_crons = new_structure_crons,
    new_structure_percent = new_structure_percent,
    crons_to_move = crons_to_move,
    to_move_percent = to_move_percent
  )
}