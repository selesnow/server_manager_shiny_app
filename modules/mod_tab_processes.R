mod_tab_processes_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Процессы",
    
    fluidRow(
      column(
        width = 12,
        div(class = "card",
            div(class = "card-header", "Фильтры и управление"),
            div(class = "card-body",
                fluidRow(
                  column(
                    width = 4,
                    selectInput(ns("filter_username"), "Пользователь", choices = NULL, multiple = TRUE),
                    selectInput(ns("filter_name"), "Имя процесса", choices = NULL, multiple = TRUE),
                    selectInput(ns("filter_client"), "Клиент", choices = NULL, multiple = TRUE),
                    selectInput(ns("filter_dir"), "Рабочая директория", choices = NULL, multiple = TRUE, width = "90%")
                  ),
                  column(
                    width = 4,
                    selectInput(ns("filter_files"), "Открытый файл", choices = NULL, multiple = FALSE, width = "100%"),
                    actionButton(ns("kill_process"), "Остановить процессы", class = "btn-danger", style = "margin-top: 10px; width: 100%")
                  )
                )
            )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        div(class = "card",
            div(class = "card-header", "Текущие процессы"),
            div(class = "card-body",
                DTOutput(ns("process_table"))
            )
        )
      )
    )
  )
}

mod_tab_processes_server <- function(id, refresh_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    processes <- reactiveVal(get_processes())
    
    observeEvent(refresh_trigger(), {
      waiter_show(
        html = HTML(paste(
          spin_fading_circles(),
          br(),
          h4("Загрузка процессов...")
        )),
        color = "#333"
      )
      processes(get_processes())
      waiter_hide()
    })
    
    observe({
      data <- processes()
      updateSelectInput(session, "filter_username", choices = unique(data$username))
      updateSelectInput(session, "filter_name", choices = unique(data$name))
      updateSelectInput(session, "filter_client", choices = unique(data$client))
    })
    
    observe({
      data <- processes()
      
      if (!is.null(input$filter_username)) {
        data <- data[data$username %in% input$filter_username, ]
      }
      if (!is.null(input$filter_name)) {
        data <- data[data$name %in% input$filter_name, ]
      }
      if (!is.null(input$filter_client)) {
        data <- data[data$client %in% input$filter_client, ]
      }
      if (!is.null(input$filter_dir)) {
        data <- data[data$dir %in% input$filter_dir, ]
      }
      
      updateSelectInput(session, "filter_dir", choices = unique(data$dir))
      updateSelectInput(session, "filter_files", choices = unique(data$files))
    })
    
    filtered_processes <- reactive({
      data <- processes()
      
      if (!is.null(input$filter_username)) {
        data <- data[data$username %in% input$filter_username, ]
      }
      if (!is.null(input$filter_name)) {
        data <- data[data$name %in% input$filter_name, ]
      }
      if (!is.null(input$filter_client)) {
        data <- data[data$client %in% input$filter_client, ]
      }
      if (!is.null(input$filter_dir)) {
        data <- data[data$dir %in% input$filter_dir, ]
      }
      
      data
    })
    
    output$process_table <- renderDT({
      datatable(filtered_processes(), filter = "top", options = list(pageLength = 25, scrollX = TRUE))
    })
    
    observeEvent(input$kill_process, {
      req(input$filter_files)
      
      data <- processes()
      target_pids <- data %>% 
        filter(files == input$filter_files) %>% 
        pull(pid) %>% 
        unique()
      
      for (pid in target_pids) {
        tryCatch({
          ps::ps_kill(ps::ps_handle(pid))
        }, error = function(e) {
          showNotification(paste("Не удалось завершить процесс", pid, ":", e$message), type = "error")
        })
      }
      
      showNotification("Процессы остановлены", type = "message")
      processes(get_processes())
    })
  })
}
