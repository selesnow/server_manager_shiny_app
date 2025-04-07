#' Запрос информации по задачам из планировщика
#'
#' @returns Таблицу с информацией по задачам
#' @export
#'
get_tasks <- function() {
  analysts_team <- dept::dp_get_team()
  analysts <- names(analysts_team)
  analyst_filter <- str_c(analysts, collapse = '|') %>% str_to_lower()
  
  taskscheduler_ls(fill = TRUE) %>%
    mutate(
      `Run As User` = str_remove_all(`Run As User`, "ANALYTICS\\\\|WIN-BTJ7HOEDRIG\\\\"),
      Author = str_remove_all(Author, "ANALYTICS\\\\|WIN-BTJ7HOEDRIG\\\\"),
      `Last Run Time` = parse_datetime(`Last Run Time`, format = "%m/%d/%Y %I:%M:%S %p")
    ) %>%
    filter(str_detect(tolower(`Run As User`), analyst_filter)) %>%
    filter(`Scheduled Task State` == "Enabled") %>% 
    mutate(`New Structure` = str_detect(`Start In`, '^C:(\\\\|/)scripts.*')) %>% 
    rowwise() %>% 
    mutate(
      Client = if_else(
        `New Structure`,
        str_split(`Start In`, pattern = '\\\\|/') %>% unlist() %>% .[4] %>% to_title_case(),
        'Unknown client'
      )
    ) %>% 
    ungroup()
  
}