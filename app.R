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
  titlePanel("Управление задачами и службами"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("author_filter"),
      uiOutput("runas_filter"),
      uiOutput("last_result_filter"),
      selectInput("selected_task", "Выберите задачу:", choices = NULL),
      actionButton("run_task", "Запустить задачу"),
      tags$hr(),
      selectInput("selected_service", "Выберите службу:", choices = NULL),
      textOutput("service_status"),
      actionButton("start_service", "Запустить"),
      actionButton("stop_service", "Остановить"),
      actionButton("restart_service", "Перезапустить")
    ),
    
    mainPanel(
      h3("Задачи"),
      DTOutput("task_table"),
      h3("Службы"),
      DTOutput("service_table")
    )
  )
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
