#' Функция для создания статистики по авторам
#'
#' @param tasks Таблица с данными по задаче
#'
#' @returns ТАблицу с данными по авторам задач
#' @export
#'
get_author_stats <- function(tasks) {
  
  tasks <- tasks %>%
    filter(`Scheduled Task State` == "Enabled") %>% 
    select(TaskName, Client, Author, `New Structure`) %>% 
    unique()
  
  tasks %>% 
    mutate(new_crons = if_else(`New Structure`, 1, 0)) %>%
    group_by(Author) %>%
    summarise(
      crons = n(),
      'new crons' = sum(new_crons)
    ) %>%
    ungroup() %>%
    mutate(
      rate = round(crons / length(unique(tasks$TaskName)) * 100, 0),
      'new cron rate' = round(`new crons` / crons * 100, 0)
    ) %>%
    arrange(desc(crons)) %>%
    select(
      Author,
      crons,
      rate,
      'new crons',
      'new cron rate'
    )
}