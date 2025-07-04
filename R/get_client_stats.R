#' Статистика по клиентам
#'
#' @param tasks Таблица с задачами
#'
#' @returns
#' @export
#'
get_client_stats <- function(tasks) {
  
  tasks <- tasks %>%
    filter(`Scheduled Task State` == "Enabled") %>% 
    select(TaskName, Client, Author, `New Structure`) %>% 
    unique()
  
  tasks %>%
    group_by(Client) %>%
    summarise(crons = n()) %>%
    ungroup() %>%
    mutate(
      rate = round(crons / length(unique(tasks$TaskName)) * 100, 0)
    ) %>%
    arrange(desc(crons))
}