library(shiny)
library(DT)
library(dplyr)
library(taskscheduleR)
library(stringr)
library(readr)
library(glue)

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
    filter(`Scheduled Task State` == "Enabled")
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
  
  # Первый вертикальный блок - фильтры и управление задачами
  fluidRow(
    column(
      width = 12,
      div(class = "card", 
          div(class = "card-header", "Фильтры и управление"),
          div(class = "card-body",
              fluidRow(
                # Блок с фильтрами
                column(
                  width = 6,
                  div(class = "mb-3", 
                      h4("Фильтры задач"),
                      uiOutput("author_filter"),
                      uiOutput("runas_filter"),
                      uiOutput("last_result_filter")
                  )
                ),
                # Блок с управлением задачами и службами
                column(
                  width = 6,
                  div(class = "mb-3",
                      h4("Управление задачами"),
                      selectInput("selected_task", "Выберите задачу:", choices = NULL),
                      div(class = "action-buttons",
                          actionButton("run_task", "Запустить", icon = icon("play"), class = "btn-primary"),
                          actionButton("view_task_logs", "Логи", icon = icon("file-alt"), class = "btn-info")
                      )
                  ),
                  tags$hr(),
                  div(
                    h4("Управление службами"),
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
      )
    )
  ),
  
  # Второй вертикальный блок - таблица задач
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
  ),
  
  # Третий вертикальный блок - таблица служб
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
    task
  })
  
  filtered_task_names <- reactive({
    task_data()$TaskName
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
  
  output$task_table <- renderDT({
    datatable(task_data(), filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  })
  
  observeEvent(input$run_task, {
    req(input$selected_task)
    taskscheduler_runnow(taskname = input$selected_task)
    showNotification(str_glue("Задача '{input$selected_task}' запущена."), type = "message")
  })
  
  services_data <- reactive({
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
    if (nrow(current) > 0) paste("Статус:", current$Status, "|", current$Description) else "Статус неизвестен"
  })
  
  observeEvent(input$start_service, {
    req(input$selected_service)
    system(str_glue("nssm start {input$selected_service}"), intern = TRUE)
    showNotification("Служба запущена", type = "message")
  })
  
  observeEvent(input$stop_service, {
    req(input$selected_service)
    system(str_glue("nssm stop {input$selected_service}"), intern = TRUE)
    showNotification("Служба остановлена", type = "warning")
  })
  
  observeEvent(input$restart_service, {
    req(input$selected_service)
    system(str_glue("nssm restart {input$selected_service}"), intern = TRUE)
    showNotification("Служба перезапущена", type = "message")
  })
}

shinyApp(ui, server, options = list(host = "0.0.0.0", port = 3838))
