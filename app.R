library(shiny)
library(DT)
library(dplyr)
library(taskscheduleR)
library(stringr)
library(readr)

# Вспомогательная функция для получения задач
get_tasks <- reactive({
  analysts_team <- dept::dp_get_team()
  analysts <- names(analysts_team)
  analyst_filter <- str_c(analysts, collapse = '|') %>% str_to_lower()
  
  task <- taskscheduleR::taskscheduler_ls(fill = TRUE) %>%
    mutate(
      `Run As User` = str_remove_all(`Run As User`, 'ANALYTICS\\\\|WIN-BTJ7HOEDRIG\\\\'),
      `Author` = str_remove_all(`Author`, 'ANALYTICS\\\\|WIN-BTJ7HOEDRIG\\\\')
    ) %>%
    mutate(`Last Run Time` = parse_datetime(`Last Run Time`, format = "%m/%d/%Y %I:%M:%S %p")) %>%
    filter(str_detect(string = tolower(`Run As User`), analyst_filter)) %>%
    filter(`Scheduled Task State` == "Enabled")
  
  task
})

# Получение служб
get_services <- reactive({
  service_data <- system("sc query state= all", intern = TRUE)
  service_lines <- grep("SERVICE_NAME: Analytics", service_data, value = TRUE)
  service_names <- str_extract(service_lines, "Analytics[\u0410-\u042F\u0430-\u044F\\w\\-_]+")
  
  service_statuses <- sapply(service_names, function(s) {
    status <- system(str_glue("sc query \"{s}\""), intern = TRUE)
    state_line <- grep("STATE", status, value = TRUE)
    str_trim(str_replace(state_line, ".*STATE.*: ", ""))
  })
  
  tibble(Service = service_names, Status = service_statuses)
})

ui <- fluidPage(
  titlePanel("Task Scheduler & Service Manager"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("author_filter"),
      uiOutput("runas_filter"),
      uiOutput("last_result_filter"),
      selectInput("selected_task", "Выберите задачу:", choices = NULL, width = '100%'),
      actionButton("run_task", "Запустить задачу"),
      hr(),
      selectInput("selected_service", "Выберите службу:", choices = NULL),
      textOutput("service_status"),
      actionButton("start_service", "Запустить"),
      actionButton("stop_service", "Остановить"),
      actionButton("restart_service", "Перезапустить")
    ),
    mainPanel(
      h3("Список задач планировщика"),
      DTOutput("task_table"),
      hr(),
      h3("Список служб"),
      DTOutput("service_table")
    )
  )
)

server <- function(input, output, session) {
  
  task_data <- reactive({
    task <- get_tasks()
    
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
    selectInput("filter_author", "Author:", choices = unique(get_tasks()$Author), multiple = TRUE)
  })
  
  output$runas_filter <- renderUI({
    selectInput("filter_runas", "Run As User:", choices = unique(get_tasks()$`Run As User`), multiple = TRUE)
  })
  
  output$last_result_filter <- renderUI({
    selectInput("filter_last_result", "Last Result:", choices = unique(get_tasks()$`Last Result`), multiple = TRUE)
  })
  
  output$task_table <- renderDT({
    datatable(task_data(), filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  })
  
  observeEvent(input$run_task, {
    req(input$selected_task)
    taskscheduler_runnow(taskname = input$selected_task)
    showNotification(str_glue("Задача '{input$selected_task}' запущена."), type = "message")
  })
  
  service_info <- reactive({
    get_services() %>% filter(Service == input$selected_service)
  })
  
  output$service_table <- renderDT({
    datatable(get_services(), options = list(pageLength = 5))
  })
  
  observe({
    updateSelectInput(session, "selected_service", choices = get_services()$Service)
  })
  
  output$service_status <- renderText({
    req(input$selected_service)
    current <- service_info()
    if (nrow(current) > 0) paste("Статус:", current$Status) else "Статус неизвестен"
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
