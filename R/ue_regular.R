library(rpup)
library(pfworker)
library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)
library(unheadr)

#' Запрос данных для расчёта юнит экономики по регулярным услугам
#'
#' @param date_from дата начала периода
#' @param date_to дата завершения периода
#'
#' @returns
#' @export
#'
#' @examples
ue_regular <- function(date_from, date_to) {
  
  date_from <- as.Date(date_from)
  date_to   <- as.Date(date_to)
  
  gs4_auth(path = Sys.getenv('SERVICE_GSHEETS_READR_ACCOUNT_PATH'))
  service_dict_dox_id <- "1wVZ-3n4OjhCcnt_h3AV5U1TVHWFGdHCUr6KzyulPTsM" 
  service_dict_sheet  <- "NC Services 2025"
  analysts_team       <- dept::dp_get_team()
  analysts            <- names(analysts_team)
  
  pup_set_config_name('replica.cfg')
  pup_connection()
  
  tasks <-  pup_get_usertask(
    pup_usernames = analysts, 
    fields = c('pf_task_id', 'project_title', 'task_link', "title")
  ) %>% 
    filter(
      str_detect(
        project_title, pattern = 'Распределение трат аналитиков|Repetitive tasks'
      )
    ) 
  
  expenses_regular <- pup_get_expenses(
    pup_usernames = analysts, 
    period        = c(format(date_from, '%Y-%m-%d 00:00:00'), format(date_to, '%Y-%m-%d 23:59:59')),
    pf_task_ids   = unique(tasks$pf_task_id)
  ) %>% 
    select(
      'username',
      'expenses_link',
      'pf_task_id',
      'hours',
      'date'
    )
  
  task_fields <- pfw_get_task_custom_data(unique(expenses_regular$pf_task_id)) %>% 
    filter(field_id %in% c('3827', '3999', '1277')) %>% 
    select(pf_task_id, field_name, text) %>% 
    mutate(
      field_name = case_when(
        field_name == 'Service name' ~ 'Название услуги',
        field_name == 'Service category' ~ 'Категория услуги',
        field_name == 'Company name' ~ 'Компания постановщика (old field)', 
        field_name == 'Назва компанії, що замовляє послугу (уточни у свого СЕО)' ~ 'Компания постановщика', 
        .default = field_name
      )
    ) %>%
    pivot_wider(id_cols = pf_task_id, names_from = field_name, values_from = text) %>% 
    mutate(
      `Компания постановщика` = coalesce(`Компания постановщика`, `Компания постановщика (old field)`)
    ) %>% 
    select(pf_task_id, `Компания постановщика`, `Категория услуги`)
  
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
  
  regular_report <- expenses_regular %>% 
    left_join(tasks, relationship = "many-to-many") %>% 
    left_join(task_fields) %>%
    ungroup() %>% 
    filter(!is.na(date)) %>% 
    mutate(
      hours = replace_na(hours, 0)
    ) %>% 
    unique() %>% 
    # mutate(
    #   title = case_when(
    #     str_detect(title, 'Поддержка автоматизации|Підтримка автоматизації')  ~ str_glue('Поддержка автоматизации (кат. {`Категория услуги`})'),
    #     str_detect(title, 'Консультации сотрудников|Консультації співробітників') ~ str_glue('Консультации сотрудников (кат. {`Категория услуги`})'),
    #     TRUE ~ 'Регулярные задачи'
    #   )
    # ) %>% 
    mutate(
      ID = case_when(
        str_detect(title, 'Поддержка автоматизации|Підтримка автоматизації')  ~ 'FC-213',
        str_detect(title, 'Консультации сотрудников|Консультації співробітників') ~ 'FC-218',
        TRUE ~ 'FC-217'
      )
    ) %>% 
    left_join(services, by = 'ID') %>% 
    filter(`Компания постановщика` != "NC Analytics team") %>% 
    mutate(
      `Категория услуги` = replace_na(`Категория услуги`, '1'),
      `Категория услуги` = if_else(`Категория услуги` == "", '1', `Категория услуги`)
    ) %>% 
    mutate(
      price = case_when(
        `Категория услуги` == '2' ~ price_cat_2,
        .default = price_cat_1
      ),
      fact_cost = round(hours * price, 2),
      plan_cost = 0
    ) %>% 
    select(
      username,
      title,
      task_link,
      project_title,
      hours,
      fact_cost,
      plan_cost,
      price
    ) %>% 
    rename(
      worker = username,
      task_title = title
    ) %>%  
    summarise(
      fact_hours = sum(hours, na.rm = T),
      fact_cost  = sum(fact_cost, na.rm = T), 
      .by = c(worker, task_title, task_link, project_title, price)
    ) %>% 
    mutate(
      task_type = "регулярные задачи"
    )
  
  return(regular_report)
  
}
