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
    ungroup() %>% 
    filter(Author != "Microsoft Visual Studio") %>% 
    # расшифровка статуса
    mutate(
      `Last Result` =  case_when(
      `Last Result` == "0"       ~ "Успешно (0)",
      `Last Result` == "267008"  ~ "Запущена (267008)",
      `Last Result` == "267009"  ~ "Задача выполняется (267009)",
      `Last Result` == "267010"  ~ "Готова к запуску (ожидает триггер) (267010)",
      `Last Result` == "267011"  ~ "Ещё не запускалас (267011)",
      `Last Result` == "267012"  ~ "Приостановлена (267012)",
      `Last Result` == "267013"  ~ "Завершена (267013)",
      `Last Result` == "267014"  ~ "Завершена с ошибкой (267014)",
      `Last Result` == "267015"  ~ "Не завершилась в отведённое время (267015)",
      `Last Result` == "1"       ~ "Ошибка: скрипт завершился аварийно (неудачное выполнение) (1)",
      `Last Result` == "2"       ~ "Ошибка: файл не найден (2)",
      `Last Result` == "3"       ~ "Ошибка: путь не найден (3)",
      `Last Result` == "5"       ~ "Ошибка: отказано в доступе (5)",
      `Last Result` == "8"       ~ "Ошибка: недостаточно памяти (8)",
      `Last Result` == "10"      ~ "Окружающая среда отсутствует (10)",
      `Last Result` == "32"      ~ "Ошибка: ресурс используется другим процессом (32)",
      `Last Result` == "87"      ~ "Ошибка: неверный параметр (87)",
      `Last Result` == "258"     ~ "Ошибка: время ожидания операции истекло (258)",
      `Last Result` == "1603"    ~ "Ошибка: установка завершилась с ошибкой (1603)",
      `Last Result` == "1639"    ~ "Ошибка: недействительный командный аргумент (1639)",
      `Last Result` == "2147942401" ~ "Ошибка: задача уже запущена (2147942401)",
      `Last Result` == "2147942402" ~ "Ошибка: задача уже завершена (2147942402)",
      `Last Result` == "2147942667" ~ "Ошибка: не найден путь к исполняемому файлу (2147942667)",
      `Last Result` == "103" ~ "Ошибка: слишком много семафоров (проблема с ресурсами синхронизации)",
      `Last Result` == "-2147483645" ~ 'Один или более аргументов указаны не верно',
      .default = str_glue("Ошибка (код {`Last Result`})")
     )
    )
  
}
