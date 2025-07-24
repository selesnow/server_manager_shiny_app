library(rpup)
library(pfworker)
library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)
library(unheadr)

ue_requested <- function(date_from, date_to) {
  
  date_from <- as.Date(date_from)
  date_to   <- as.Date(date_to)
  
  analysts            <- dept::dp_get_team() %>% names()
  period              <- timeperiodsR::this_month()
  custom_fields_ids   <- c('3993', '3999', '4203', '3827')
  service_dict_dox_id <- "1wVZ-3n4OjhCcnt_h3AV5U1TVHWFGdHCUr6KzyulPTsM" 
  service_dict_sheet  <- "NC Services 2025"
  
  gs4_auth(path = Sys.getenv('SERVICE_GSHEETS_READR_ACCOUNT_PATH'))
  
  # load data ---------------------------------------------------------------
  pup_set_config_name('replica.cfg')
  pup_connection()
  
  tasks <- pup_get_usertask(
    #task_complete_dates = c(format(period$start, '%Y-%m-%d 00:00:00'), format(period$end, '%Y-%m-%d 23:59:59')),
    pup_usernames       = analysts, 
    fields              = c(
      'pf_task_id', 'task_link', 'title', 'project_title', 'start_date', 'done_date',
      'complete_date', 'planfix_tasks_delete', 'owner_user_id', 'task_status', "username as task_worker"
    )
  ) %>% 
    mutate(complete_date = complete_date - lubridate::hours(5)) %>% 
    filter(between(as.Date(complete_date), date_from, date_to)) %>%
    filter(planfix_tasks_delete == 0) %>% 
    filter(task_status %in% c("Завершенная", "Complete", "Отмененная", "Canceled")) %>% 
    select(-planfix_tasks_delete) 
  
  if (nrow(tasks) > 0) {
    expenses_regular <- pup_get_expenses(
      pup_usernames = analysts, 
      period        = c(as.Date('2014-09-01'), Sys.Date())
    ) %>% 
      group_by(pf_task_id) %>% 
      summarise(fact_hours = sum(hours))
    
    requestors <- pup_get_user_info(with_fired_users = T, fields = c('user_id', 'username')) %>% 
      filter(user_id %in% unique(tasks$owner_user_id)) %>% 
      rename(owner = username)
    
    # аналитики оп задачам
    if ( length(tasks$pf_task_id) > 0 ) {
      analytics <- retry({pfw_get_tasks_analytics(unique(tasks$pf_task_id)) %>% 
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
    
    task_fields <- pfw_get_task_custom_data(unique(tasks$pf_task_id)) %>% 
      filter(field_id %in% custom_fields_ids) %>% 
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
      mutate(
        Amount = if_else(Amount == "", "1",  Amount),
        Amount = replace_na(Amount, "1"),
        Amount = as.numeric(Amount)
      )
    
    tasks <- left_join(tasks, requestors, by = c("owner_user_id" = "user_id"))
    
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
    
    # requested tasks
    requesdet_services <- tasks %>% 
      left_join(expenses_regular) %>% 
      filter(str_detect(project_title, pattern = 'Sprint |AT Iteration Tasks')) %>% 
      left_join(task_fields) %>% 
      left_join(analytics, join_by(pf_task_id == task_id)) %>% 
      left_join(services, by = 'ID') %>% 
      filter(! is.na(task_worker)) %>% 
      filter(`Компания постановщика` != "NC Analytics team") %>% 
      mutate(
        fact_hours = if_else(`Название услуги` == "Створення одноразових вивантажень з N1/ ПУПу", Amount, fact_hours),
        price = case_when(
          `Категория услуги` == '2' ~ price_cat_2,
          .default = price_cat_1
        ),
        fact_cost = round(fact_hours * price, 2),
        plan_cost = round(total_plan_score * price, 2)
      ) %>% 
      select(
        task_worker,
        title,
        task_link,
        task_status,
        project_title,
        owner,
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
        worker = task_worker,
        task_title = title,
        task_owner = owner,
        plan_hours = total_plan_score
      ) %>% 
      mutate(
        task_type = "заказные задачи"
      )
    
    return(requesdet_services)
    
  }
  
  return('Данные для отчёта не найдены!')
  
}

