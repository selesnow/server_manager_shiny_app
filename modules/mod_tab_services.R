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
                      actionButton(ns("start_service"), "Запустить", icon = icon("play"), class = "btn-success"),
                      actionButton(ns("stop_service"), "Остановить", icon = icon("stop"), class = "btn-danger"),
                      actionButton(ns("restart_service"), "Перезапустить", icon = icon("sync"), class = "btn-warning")
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
                DTOutput(ns("service_table"))
            )
        )
      )
    )
  )
}

mod_tab_services_server <- function(id, services_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    service_info <- reactive({
      req(input$selected_service)
      services_data() %>% filter(Service == input$selected_service)
    })
    
    output$service_table <- renderDT({
      datatable(services_data(), options = list(pageLength = 5))
    })
    
    observeEvent(services_data(), {
      req(services_data())
      updateSelectInput(session, "selected_service", choices = services_data()$Service)
    })
    
    output$service_status <- renderText({
      current <- service_info()
      if (nrow(current) > 0) paste("Статус:", current$Status, "|", current$Description) else "Статус неизвестен"
    })
    
    observeEvent(input$start_service, {
      req(input$selected_service)
      system(glue::glue("nssm start {input$selected_service}"), intern = TRUE)
      showNotification("Служба запущена", type = "message")
    })
    
    observeEvent(input$stop_service, {
      req(input$selected_service)
      system(glue::glue("nssm stop {input$selected_service}"), intern = TRUE)
      showNotification("Служба остановлена", type = "warning")
    })
    
    observeEvent(input$restart_service, {
      req(input$selected_service)
      system(glue::glue("nssm restart {input$selected_service}"), intern = TRUE)
      showNotification("Служба перезапущена", type = "message")
    })
  })
}
