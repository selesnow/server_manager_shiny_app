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
                    width = 8,
                    selectInput(ns("filter_username"), "Пользователь", choices = NULL, multiple = TRUE),
                    selectInput(ns("filter_dir"), "Рабочая директория", choices = NULL, multiple = TRUE, width = "100%"),
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
      updateSelectInput(session, "filter_dir", choices = unique(data$dir))
    })
    
    # Динамически подгружаем список файлов, в зависимости от username и dir
    observe({
      data <- processes()
      
      if (!is.null(input$filter_username)) {
        data <- data[data$username %in% input$filter_username, ]
      }
      if (!is.null(input$filter_dir)) {
        data <- data[data$dir %in% input$filter_dir, ]
      }
      
      updateSelectInput(session, "filter_files", choices = unique(data$files))
    })
    
    filtered_processes <- reactive({
      data <- processes()
      
      if (!is.null(input$filter_username)) {
        data <- data[data$username %in% input$filter_username, ]
      }
      if (!is.null(input$filter_dir)) {
        data <- data[data$dir %in% input$filter_dir, ]
      }
      if (!is.null(input$filter_files)) {
        data <- data[data$files == input$filter_files, ]
      }
      
      data
    })
    
    output$process_table <- renderDT({
      datatable(filtered_processes(), options = list(pageLength = 10, scrollX = TRUE))
    })
    
    observeEvent(input$kill_process, {
      req(input$filter_files)
      
      pids <- filtered_processes() %>% pull(pid) %>% unique()
      
      for (pid in pids) {
        tryCatch({
          ps::ps_kill(ps::ps_handle(pid))
        }, error = function(e) {
          showNotification(paste("Не удалось завершить процесс", pid, ":", e$message), type = "error")
        })
      }
      
      showNotification("Процессы остановлены", type = "message")
      
      processes(get_processes())  # Обновим после остановки
    })
  })
  
}
