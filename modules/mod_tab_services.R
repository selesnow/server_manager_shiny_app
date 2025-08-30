mod_tab_services_ui <- function(id) {
  ns <- NS(id)
  
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
                  selectInput(ns("selected_service"), "Выберите службу:", choices = NULL),
                  textOutput(ns("service_status")),
                  div(class = "action-buttons",
                      uiOutput(ns("service_buttons"))
                  )
                )
            ),
            textOutput(ns("last_update"))
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
                DTOutput(ns("service_table"))
            )
        )
      )
    )
  )
}

mod_tab_services_server <- function(id, services_data, user_role, auth, session_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    service_info <- reactive({
      req(input$selected_service)
      services_data() %>% filter(Service == input$selected_service)
    })
    
    output$service_table <- renderDT({
      datatable(services_data() %>% select(-update_time), ,
                filter   = "top",
                options  = list(pageLength = 25, scrollX = TRUE),
                selection = 'none')
    })
    
    observeEvent(services_data(), {
      req(services_data())
      updateSelectInput(session, "selected_service", choices = services_data()$Service)
    })
    
    output$service_status <- renderText({
      current <- service_info()
      if (nrow(current) > 0) paste("Статус:", current$Status, "|", current$Description) else "Статус неизвестен"
    })
    
    output$service_buttons <- renderUI({
      role <- user_role()
      
      if (role %in% c("admin", "user")) {
        tagList(
          actionButton(ns("start_service"), "Запустить", icon = icon("play"), class = "btn-success"),
          actionButton(ns("stop_service"), "Остановить", icon = icon("stop"), class = "btn-danger"),
          actionButton(ns("restart_service"), "Перезапустить", icon = icon("sync"), class = "btn-warning")
        )
      }
    })
    
    # Время обновления данных
    output$last_update <- renderText({
      data <- services_data()
      req(data)
      
      last_time <- max(data$update_time, na.rm = TRUE)
      paste0("Данные обновлены: ", format(last_time, "%Y-%m-%d %H:%M:%S %Z"))
    })
    
    observeEvent(input$start_service, {
      write_action_log(user = auth$user()$login, func = 'Service start', session_id, value = input$selected_service)
      req(input$selected_service)
      system(glue::glue("nssm start {input$selected_service}"), intern = TRUE)
      showNotification("Служба запущена", type = "message")
    })
    
    observeEvent(input$stop_service, {
      write_action_log(user = auth$user()$login, func = 'Service stop', session_id, value = input$selected_service)
      req(input$selected_service)
      system(glue::glue("nssm stop {input$selected_service}"), intern = TRUE)
      showNotification("Служба остановлена", type = "warning")
    })
    
    observeEvent(input$restart_service, {
      write_action_log(user = auth$user()$login, func = 'Service restart', session_id, value = input$selected_service)
      req(input$selected_service)
      system(glue::glue("nssm restart {input$selected_service}"), intern = TRUE)
      showNotification("Служба перезапущена", type = "message")
    })
  })
}
