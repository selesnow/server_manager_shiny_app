library(stringr)
library(rpup)
library(pfworker)
library(dplyr)
library(tidyr)
library(readr)
library(retry)
library(unheadr)
library(rlist)

#' Получить задачи из планфикса по рабочему спринту команды аналитиков за указанный месяц
#'
#' @param year Год за который надо будет получить данные
#' @param month Месяц года за который надо будет получить данные
#'
#' @returns Таблица с задачами указанного спринта команды аналитиков
#' @export
get_sprint_planfix_tasks <- function(
    year = format(Sys.Date(), "%y"),
    month = format(Sys.Date(), "%m")  
  )
  {
  analysts_team <- dept::dp_get_team(statuses = 'active')
  analysts <- names(analysts_team)
  analysts_tgs <- list.mapv(analysts_team, telegram) %>%
    c('haruka_np') %>%
    str_replace_all('_', '\\\\_') %>% 
    str_c("@", ., collapse = " ")
  
  # подключение к пупу
  pup_set_config_name('replica.cfg')
  pup_connection()
  
  # запрашиваем данные по задачам сотрудников отдела аналитики
  tasks <- pup_get_usertask(
    pup_usernames = analysts
  ) %>% 
    filter(planfix_tasks_delete == 0)
  
  current_sprint_project_title <- str_glue('{year}/{month} NC AT Iteration Tasks') 
  
  current_sprint <- filter(tasks, project_title == current_sprint_project_title)
  
  # запрашиваем траты по задачам
  if ( length(current_sprint$pf_task_id) > 0 ) {
    expenses <- pup_get_expenses(
      pup_usernames = analysts, 
      pf_task_ids   = current_sprint$pf_task_id, 
      period        = c(as.Date('2021-01-01'), Sys.Date())
    ) %>% 
      group_by(pf_task_id) %>% 
      summarise(fact_hours = sum(hours))
    
  } 
  
  # отключаемся от пупа
  pup_disconnect()
  
  # аналитики оп задачам
  if ( length(current_sprint$pf_task_id) > 0 ) {
    analytics <- retry({pfw_get_tasks_analytics(current_sprint$pf_task_id) %>% 
        filter(itemData_id == "20") %>%
        select(task_id, itemData_value) %>% 
        mutate(total_plan_score = parse_number(itemData_value)) %>% 
        group_by(task_id) %>% 
        summarise(total_plan_score = sum(total_plan_score)) %>% 
        mutate(task_id = as.integer(task_id)) }, 
        until =  ~ "data.frame" %in% class(.), 
        interval = 30, 
        max_tries = 3)
  }
  
  service_dict_dox_id <- '1wVZ-3n4OjhCcnt_h3AV5U1TVHWFGdHCUr6KzyulPTsM'#"1i-0rAtoll-36olpFqn7V49s5fRLwpoQ2t2o4a3jbHlY" 
  service_dict_sheet  <- "NC Services 2025"
  gs4_auth(path = Sys.getenv('SERVICE_GSHEETS_READR_ACCOUNT_PATH'))
  services <- range_read(
    service_dict_dox_id, 
    sheet = service_dict_sheet, 
    range = "A1:AG",
    na = "-"
  ) %>% 
    mash_colnames(n_name_rows = 1) %>% 
    rename_with(str_remove_all ,pattern =  '...\\d{2}_') %>% 
    filter(!is.na(ID)) %>% 
    filter(!is.na(`ЦІНА 1 послуги, Б_ЦІНА 1 категорії`)) %>% 
    filter(!is.null(`ЦІНА 1 послуги, Б_ЦІНА 1 категорії`)) %>% 
    select(
      ID, 
      Послуга, 
      `Тип послуги: позамовна/регулярна`, 
      `Віднесення кількості продажів на клієнта: напряму чи розподіл`, 
      `Одиниця виміру продажів`,
      `ЦІНА 1 послуги, Б_ЦІНА 1 категорії`, 
      `ЦІНА 2 категорії`, 
      `ЦІНА 3 категорії`,
      `Назва категорії 1`,
      `Назва категорії 2`,
      `Назва категорії 3`
    ) %>% 
    rename(
      Услуга = Послуга, 
      `Тип услуги:\nПозаказная / Регулярная` = `Тип послуги: позамовна/регулярна`, 
      `ЦЕНЫ, Б_ЦЕНА 1 услуги Категории 1` = `ЦІНА 1 послуги, Б_ЦІНА 1 категорії`, 
      `Единица измерения продаж` = `Одиниця виміру продажів`,
      `Способ отнесения количества оказанной услуги (продаж) на конкретного внутреннего клиента\n// Напрямую или Распределение`= `Віднесення кількості продажів на клієнта: напряму чи розподіл`,
      `ЦЕНА 1 услуги Категории 2` = `ЦІНА 2 категорії`, 
      `ЦЕНА 1 услуги Категории 3` = `ЦІНА 3 категорії`,
      `Название категории 1` = `Назва категорії 1`,
      `Название категории 2` = `Назва категорії 2`,
      `Название категории 3` = `Назва категорії 3`
    ) %>% 
    mutate(
      `ЦЕНЫ, Б_ЦЕНА 1 услуги Категории 1` = purrr::map_dbl(`ЦЕНЫ, Б_ЦЕНА 1 услуги Категории 1`, ~ if (!is.null(.x)) as.numeric(.x) else NA) , 
      `ЦЕНА 1 услуги Категории 2` = purrr::map_dbl(`ЦЕНА 1 услуги Категории 2`,  ~ if (!is.null(.x)) as.numeric(.x) else NA), #readr::parse_double(`ЦЕНА 1 услуги Категории 2`, locale = readr::locale(decimal_mark = ',')),
      `ЦЕНА 1 услуги Категории 3` = purrr::map_dbl(`ЦЕНА 1 услуги Категории 3`,  ~ if (!is.null(.x)) as.numeric(.x) else NA)  #readr::parse_double(`ЦЕНА 1 услуги Категории 3`, locale = readr::locale(decimal_mark = ','))
    ) %>% 
    select(
      ID, 
      `ЦЕНЫ, Б_ЦЕНА 1 услуги Категории 1`,
      `ЦЕНА 1 услуги Категории 2`
    ) %>% 
    rename(
      price_cat_1 = `ЦЕНЫ, Б_ЦЕНА 1 услуги Категории 1`,
      price_cat_2 = `ЦЕНА 1 услуги Категории 2`
    )
  
  if ( length(current_sprint$pf_task_id) > 0 ) {
    custom_data <- pfw_get_task_custom_data(current_sprint$pf_task_id) %>% 
      filter(field_id %in% c('1277', '3993', '3999')) %>%
      mutate(
        field_name = case_when(
          field_name == 'Service name' ~ 'Название услуги',
          field_name == 'Service category' ~ 'Категория услуги',
          field_name == 'Company name' ~ 'Компания постановщика', 
          field_name == 'Назва компанії, що замовляє послугу (уточни у свого СЕО)' ~ 'Компания постановщика',
          .default = field_name
        )
      ) %>%
      select(pf_task_id, field_name, text) %>% 
      pivot_wider(id_cols = pf_task_id, names_from = field_name, values_from = text) %>% 
      #rename('Название услуги' = 'Название услуги департамента аналитики') %>% 
      separate(`Название услуги`, c('Название услуги', 'ID'), sep = ' / ') %>% 
      select(pf_task_id, ID, 'Категория услуги')
  }
  
  # объединяем данные
  if ( length(current_sprint$pf_task_id) > 0 ) {
    res <- left_join(current_sprint, expenses, by = 'pf_task_id') %>% 
      left_join(analytics, by = c('pf_task_id' = 'task_id')) %>% 
      left_join(custom_data, by = 'pf_task_id') %>% 
      left_join(services, by = 'ID') %>% 
      mutate(
        price = case_when(
          `Категория услуги` == '2' ~ price_cat_2,
          .default = price_cat_1
        ),
        fact_cost = round(fact_hours * price, 2),
        plan_cost = round(total_plan_score * price, 2)
      ) %>% 
    select(
      username,
      title,
      task_link,
      task_status,
      project_title,
      pf_owner_name,
      start_date,
      done_date,
      complete_date,
      fact_hours,
      total_plan_score,
      price,
      fact_cost,
      plan_cost
    ) %>% 
      rename(
        worker = username,
        task_title = title,
        task_owner = pf_owner_name,
        plan_hours = total_plan_score
      ) %>% 
      readr::format_csv()
  } else {
    res <- "На данный момент нет данных по задачам текущего спринта."
  }
}
