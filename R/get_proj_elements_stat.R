#' Функция для создания статистики по авторам
#'
#' @param tasks Таблица с данными по задаче
#'
#' @returns ТАблицу с данными по авторам задач
#' @export
#'
get_proj_elements_stat <- function(tasks) {
  
  tasks <- tasks %>%
    filter(`Scheduled Task State` == "Enabled") %>% 
    select(TaskName, Author, readme, news, git, rproj, has_log) %>% 
    unique()
  
  tasks %>% 
    group_by(Author) %>%
    summarise(
      crons  = n(),
      readme = sum(readme),
      news   = sum(news),
      git    = sum(git),
      rproj  = sum(rproj),
      logs   = sum(has_log)
    ) %>%
    ungroup() %>%
    mutate(
      `readme rate` = round(readme / crons * 100, 0),
      `news rate`   = round(news / crons * 100, 0),
      `git rate`    = round(git / crons * 100, 0),
      `rproj rate`  = round(rproj / crons * 100, 0),
      `logs rate`   = round(logs / crons * 100, 0)
    ) %>%
    arrange(desc(crons)) %>%
    select(
      Author,
      crons,
      readme, 'readme rate',
      news, 'news rate',
      git, 'git rate',
      rproj, 'rproj rate',
      logs, 'logs rate'
    )
}
