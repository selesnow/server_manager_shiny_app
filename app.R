library(shiny)
library(shinyjs)  # Добавляем библиотеку shinyjs
library(DT)
library(dplyr)
library(taskscheduleR)
library(stringr)
library(readr)
library(glue)
library(snakecase)

# Функция поиска и чтения логов
# Функция поиска и чтения логов
find_log <- function(task_to_run = NULL, start_in = NULL) {  # Исправление: null -> NULL
  
  file_ext <- tools::file_ext(unique(trimws(task_to_run)))
  
  if (tolower(file_ext) == 'r') {
    
    r_file    <- strsplit(unique(task_to_run), split = ' ')[[1]] %>% 
      .[length(.)]
    rout_path <- str_glue('{start_in}\\{r_file}out')
    
    rout_file  <- readLines(unique(rout_path)) %>% 
      str_c(., collapse = '\n')
    
    return(rout_file)
    
  }
  
  if (tolower(file_ext) == 'bat') {
    
    bat_file  <- readLines(unique(task_to_run)) %>% 
      str_c(., collapse = '\n')
    
    bat_file  <- str_glue(bat_file)
    
    return(bat_file)
    
  }
  
  return('Лог не найден!')
  
}

# Получение задач (обычная функция)
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
    rowwise() %>% 
    mutate(
      Client = str_split(`Start In`, pattern = '\\\\|/') %>% unlist() %>% .[4] %>% to_title_case()
    ) %>% 
    ungroup()
  
}

# Получение служб с описанием
get_services <- function() {
  service_data <- system("sc query state= all", intern = TRUE)
  service_lines <- grep("SERVICE_NAME: Analytics", service_data, value = TRUE)
  service_names <- str_extract(service_lines, "Analytics[А-Яа-я\\w\\-_]+")
  
  tibble(Service = service_names) %>%
    rowwise() %>%
    mutate(
      Status = {
        status <- system(glue("sc query \"{Service}\""), intern = TRUE)
        state_line <- grep("STATE", status, value = TRUE)
        str_trim(str_replace(state_line, ".*STATE.*: ", ""))
      },
      DisplayName = {
        info <- system(glue("sc qc \"{Service}\""), intern = TRUE)
        display_line <- grep("DISPLAY_NAME", info, value = TRUE)
        if (length(display_line) > 0) {
          str_trim(str_remove(display_line, "DISPLAY_NAME *:"))
        } else {
          "Нет DisplayName"
        }
      },
      Description = {
        desc_info <- suppressWarnings(system(glue("nssm get \"{Service}\" Description"), intern = TRUE))
        if (length(desc_info) > 0 && desc_info != "") {
          str_trim(desc_info)
        } else {
          "Нет описания"
        }
      }
    ) %>%
    ungroup()
}

ui <- fluidPage(
  useShinyjs(),  # Добавляем использование shinyjs
  
  # Добавляем возможность переключения темной темы
  tags$head(
    tags$style(HTML("
      body {
        background-color: #333;
        color: #f5f5f5;
      }
      .card {
        background-color: #444;
        border-color: #555;
        margin-bottom: 20px;
      }
      .card-header {
        background-color: #555;
        border-color: #666;
      }
      .btn-primary {
        background-color: #007bff;
      }
      .btn-success {
        background-color: #28a745;
      }
      .btn-danger {
        background-color: #dc3545;
      }
      .btn-warning {
        background-color: #ffc107;
        color: #333;
      }
      .btn-info {
        background-color: #17a2b8;
      }
      select, input {
        background-color: #555;
        color: #f5f5f5;
        border-color: #666;
      }
      table {
        color: #f5f5f5;
      }
      .dataTables_wrapper {
        color: #f5f5f5;
      }
      .dataTables_info, 
      .dataTables_paginate,
      .dataTables_filter,
      .dataTables_length {
        color: #f5f5f5 !important;
      }
      
      /* Стили для светлой темы */
      .light-mode {
        background-color: #ffffff;
        color: #333;
      }
      .light-mode .card {
        background-color: #f9f9f9;
        border-color: #ddd;
      }
      .light-mode .card-header {
        background-color: #f1f1f1;
        border-color: #ddd;
      }
      .light-mode select, .light-mode input {
        background-color: #ffffff;
        color: #333;
        border-color: #ccc;
      }
      .light-mode table {
        color: #333;
      }
      .light-mode .dataTables_wrapper {
        color: #333;
      }
      .light-mode .dataTables_info, 
      .light-mode .dataTables_paginate,
      .light-mode .dataTables_filter,
      .light-mode .dataTables_length {
        color: #333 !important;
      }
      
      /* Стиль для светлой темы логов */
      .light-mode .light-mode-log {
        background-color: #f0f0f0 !important;
        color: #333 !important;
      }
      
      /* Стиль для header */
      .header-container {
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      .controls-container {
        display: flex;
        align-items: center;
      }
      
      /* Стиль для вкладок */
      .nav-tabs {
        border-bottom: 1px solid #555;
      }
      .nav-tabs .nav-link {
        color: #f5f5f5;
        background-color: #444;
        border-color: #555;
      }
      .nav-tabs .nav-link.active {
        color: #f5f5f5;
        background-color: #555;
        border-color: #666;
      }
      .light-mode .nav-tabs .nav-link {
        color: #333;
        background-color: #f1f1f1;
        border-color: #ddd;
      }
      .light-mode .nav-tabs .nav-link.active {
        color: #333;
        background-color: #ffffff;
        border-color: #ddd;
      }
    "))
  ),
  
  # Используем правильную структуру заголовка без встроенных стилей
  titlePanel(
    title = "Server Task & Service Manager",
    windowTitle = "Server Task & Service Manager"
  ),
  
  # Добавляем элементы управления отдельно, после заголовка
  div(class = "header-container",
      div(), # Пустой элемент для правильного выравнивания
      div(class = "controls-container",
          checkboxInput("dark_mode", "Светлая тема", FALSE),
          actionButton("refresh_data", "Обновить данные", icon = icon("refresh"), class = "btn-sm ml-2")
      )
  ),
  
  # Начало вкладок
  tabsetPanel(
    id = "main_tabs",
    
    # Вкладка "Задачи"
    tabPanel(
      title = "Задачи",
      
      # Первый вертикальный блок - фильтры и управление задачами
      fluidRow(
        column(
          width = 12,
          div(class = "card", 
              div(class = "card-header", "Фильтры и управление задачами"),
              div(class = "card-body",
                  fluidRow(
                    # Блок с фильтрами
                    column(
                      width = 6,
                      div(class = "mb-3", 
                          h4("Фильтры задач"),
                          uiOutput("author_filter"),
                          uiOutput("runas_filter"),
                          uiOutput("last_result_filter"),
                          uiOutput("client_filter") # Добавляем фильтр по клиенту
                      )
                    ),
                    # Блок с управлением задачами
                    column(
                      width = 6,
                      div(class = "mb-3",
                          h4("Управление задачами"),
                          selectInput("selected_task", "Выберите задачу:", choices = NULL),
                          div(class = "action-buttons",
                              actionButton("run_task", "Запустить", icon = icon("play"), class = "btn-success"),
                              actionButton("view_task_logs", "Логи", icon = icon("file-alt"), class = "btn-info")
                          )
                      )
                    )
                  )
              )
          )
        )
      ),
      
      # Блок с выводом лога
      fluidRow(
        column(
          width = 12,
          div(class = "card", style = "display: none;", id = "log_card",
              div(class = "card-header", "Логи задачи"),
              div(class = "card-body",
                  h4(textOutput("log_task_name")),
                  tags$div(
                    style = "background-color: #2a2a2a; color: #ddd; padding: 10px; border-radius: 5px; max-height: 400px; overflow-y: auto;",
                    class = "light-mode-log",
                    verbatimTextOutput("task_log_content")
                  )
              )
          )
        )
      ),
      
      # Таблица задач
      fluidRow(
        column(
          width = 12,
          div(class = "card",
              div(class = "card-header", "Задачи"),
              div(class = "card-body",
                  DTOutput("task_table")
              )
          )
        )
      )
    ),
    
    # Вкладка "Службы"
    tabPanel(
      title = "Службы",
      
      # Блок с управлением службами
      fluidRow(
        column(
          width = 12,
          div(class = "card", 
              div(class = "card-header", "Управление службами"),
              div(class = "card-body",
                  div(
                    h4("Выбор и управление службами"),
                    selectInput("selected_service", "Выберите службу:", choices = NULL),
                    textOutput("service_status"),
                    div(class = "action-buttons",
                        actionButton("start_service", "Запустить", icon = icon("play"), class = "btn-success"),
                        actionButton("stop_service", "Остановить", icon = icon("stop"), class = "btn-danger"),
                        actionButton("restart_service", "Перезапустить", icon = icon("sync"), class = "btn-warning")
                    )
                  )
              )
          )
        )
      ),
      
      # Таблица служб
      fluidRow(
        column(
          width = 12,
          div(class = "card",
              div(class = "card-header", "Службы"),
              div(class = "card-body",
                  DTOutput("service_table")
              )
          )
        )
      )
    )
  ),
  
  # Добавляем CSS для кнопок действий
  tags$head(
    tags$style(HTML("
      .action-buttons {
        display: flex;
        gap: 10px;
        flex-wrap: wrap;
        margin-top: 10px;
      }
    "))
  ),
  
  # JavaScript для переключения темной/светлой темы
  tags$script(HTML("
    $(document).ready(function() {
      // Темная тема по умолчанию
      $('#dark_mode').on('change', function() {
        if($(this).is(':checked')) {
          $('body').addClass('light-mode');
        } else {
          $('body').removeClass('light-mode');
        }
      });
    });
  "))
)

server <- function(input, output, session) {
  all_tasks <- reactiveVal(NULL)
  
  # Добавляем реактивное значение для отслеживания обновлений
  refresh_trigger <- reactiveVal(0)
  
  observe({
    all_tasks(get_tasks())
  })
  
  task_data <- reactive({
    req(all_tasks())
    task <- all_tasks()
    
    if (!is.null(input$filter_author)) {
      task <- task %>% filter(Author %in% input$filter_author)
    }
    if (!is.null(input$filter_runas)) {
      task <- task %>% filter(`Run As User` %in% input$filter_runas)
    }
    if (!is.null(input$filter_last_result)) {
      task <- task %>% filter(`Last Result` %in% input$filter_last_result)
    }
    if (!is.null(input$filter_client)) {
      task <- task %>% filter(Client %in% input$filter_client)
    }
    
    task
  })
  
  filtered_task_names <- reactive({
    task_data()$TaskName
  })
  
  # Поиск и чтение лога
  observeEvent(input$view_task_logs, {
    req(input$selected_task)
    
    # Находим выбранную задачу в таблице
    selected_task_data <- all_tasks() %>% 
      filter(TaskName == input$selected_task)
    
    if(nrow(selected_task_data) > 0) {
      # Получаем нужные поля
      task_to_run <- selected_task_data$`Task To Run`
      start_in <- selected_task_data$`Start In`
      
      # Если данные получены, вызываем функцию find_log (с чтением последних 25 строк)
      if(!is.null(task_to_run) && !is.null(start_in)) {
        log_content <- try(find_log(task_to_run = task_to_run, start_in = start_in), silent = TRUE)
        
        if(inherits(log_content, "try-error")) {
          output$task_log_content <- renderText({ "Ошибка при чтении лога" })
        } else {
          output$task_log_content <- renderText({ log_content })
        }
        
        # Показываем имя задачи
        output$log_task_name <- renderText({ paste("Лог задачи:", input$selected_task) })
        
        # Показываем блок с логами
        shinyjs::show(id = "log_card")
      } else {
        showNotification("Не удалось найти данные для задачи", type = "error")
      }
    } else {
      showNotification("Задача не найдена", type = "error")
    }
  })
  
  observe({
    updateSelectInput(session, "selected_task", choices = filtered_task_names())
  })
  
  output$author_filter <- renderUI({
    req(all_tasks())
    selectInput("filter_author", "Author:", choices = unique(all_tasks()$Author), multiple = TRUE)
  })
  
  output$runas_filter <- renderUI({
    req(all_tasks())
    selectInput("filter_runas", "Run As User:", choices = unique(all_tasks()$`Run As User`), multiple = TRUE)
  })
  
  output$last_result_filter <- renderUI({
    req(all_tasks())
    selectInput("filter_last_result", "Last Result:", choices = unique(all_tasks()$`Last Result`), multiple = TRUE)
  })
  
  # Добавляем фильтр по клиенту
  output$client_filter <- renderUI({
    req(all_tasks())
    selectInput("filter_client", "Client:", choices = unique(all_tasks()$Client), multiple = TRUE)
  })
  
  output$task_table <- renderDT({
    datatable(task_data(), filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  })
  
  observeEvent(input$run_task, {
    req(input$selected_task)
    taskscheduler_runnow(taskname = input$selected_task)
    showNotification(str_glue("Задача '{input$selected_task}' запущена."), type = "message")
  })
  
  # Модифицируем реактивное значение services_data, чтобы оно зависело от refresh_trigger
  services_data <- reactive({
    # Это заставит services_data пересчитываться каждый раз при изменении refresh_trigger
    refresh_trigger()
    get_services()
  })
  
  service_info <- reactive({
    services_data() %>% filter(Service == input$selected_service)
  })
  
  output$service_table <- renderDT({
    datatable(services_data(), options = list(pageLength = 5))
  })
  
  observe({
    updateSelectInput(session, "selected_service", choices = services_data()$Service)
  })
  
  output$service_status <- renderText({
    req(input$selected_service)
    current <- service_info()
    if (nrow(current) > 0)
      paste("Статус:", current$Status, "|", current$Description)
    else
      "Статус неизвестен"
  })
  
  observeEvent(input$start_service, {
    req(input$selected_service)
    system(str_glue("nssm start {input$selected_service}"), intern = TRUE)
    showNotification("Служба запущена", type = "message")
    # Обновляем триггер для обновления данных о службах
    refresh_trigger(refresh_trigger() + 1)
  })
  
  observeEvent(input$stop_service, {
    req(input$selected_service)
    system(str_glue("nssm stop {input$selected_service}"), intern = TRUE)
    showNotification("Служба остановлена", type = "warning")
    # Обновляем триггер для обновления данных о службах
    refresh_trigger(refresh_trigger() + 1)
  })
  
  observeEvent(input$restart_service, {
    req(input$selected_service)
    system(str_glue("nssm restart {input$selected_service}"), intern = TRUE)
    showNotification("Служба перезапущена", type = "message")
    # Обновляем триггер для обновления данных о службах
    refresh_trigger(refresh_trigger() + 1)
  })
  
  # Модифицируем обработчик для кнопки обновления данных
  observeEvent(input$refresh_data, {
    # Обновляем данные о задачах
    all_tasks(get_tasks())
    
    # Увеличиваем значение refresh_trigger, что вызовет перерасчет services_data()
    refresh_trigger(refresh_trigger() + 1)
    
    # Показываем уведомление об успешном обновлении
    showNotification("Данные успешно обновлены", type = "message", duration = 3)
  })
  
  # Добавим обработчик для поиска в таблице задач, если он нужен
  filtered_task_data <- reactive({
    data <- task_data()
    
    if (!is.null(input$task_search) && input$task_search != "") {
      search_term <- tolower(input$task_search)
      data <- data[apply(data, 1, function(row) any(grepl(search_term, tolower(row), fixed = TRUE))), ]
    }
    
    return(data)
  })
  
  # Заменим обработчик таблицы задач, чтобы использовать фильтрацию
  output$task_table <- renderDT({
    datatable(filtered_task_data(), filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Добавим обработчик для поиска в таблице служб
  filtered_service_data <- reactive({
    data <- services_data()
    
    if (!is.null(input$service_search) && input$service_search != "") {
      search_term <- tolower(input$service_search)
      data <- data[apply(data, 1, function(row) any(grepl(search_term, tolower(row), fixed = TRUE))), ]
    }
    
    return(data)
  })
  
  # Заменим обработчик таблицы служб, чтобы использовать фильтрацию
  output$service_table <- renderDT({
    datatable(filtered_service_data(), options = list(pageLength = 5))
  })
}

shinyApp(ui, server, options = list(host = "0.0.0.0", port = 3838))