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
                    selectInput(ns("filter_dir"), "Рабочая директория", choices = NULL, multiple = TRUE, width = "90%"),
                    div(class = "card-footer", style = "margin-top: 5px; font-size: 0.9em; color: #bbb;", textOutput(ns("last_update")))
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

mod_tab_processes_server <- function(id, process_data, auth, session_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Реактивка для фильтрации файлов — зависит от других фильтров
    processes_filtered_for_files <- reactive({
      data <- process_data()
      req(data)
      
      if (!is.null(input$filter_username) && length(input$filter_username) > 0) {
        data <- data[data$username %in% input$filter_username, ]
      }
      if (!is.null(input$filter_name) && length(input$filter_name) > 0) {
        data <- data[data$name %in% input$filter_name, ]
      }
      if (!is.null(input$filter_client) && length(input$filter_client) > 0) {
        data <- data[data$client %in% input$filter_client, ]
      }
      if (!is.null(input$filter_dir) && length(input$filter_dir) > 0) {
        data <- data[data$dir %in% input$filter_dir, ]
      }
      
      data
    })
    
    # Окончательная таблица — применяем все фильтры
    filtered_processes <- reactive({
      data <- processes_filtered_for_files()
      
      if (!is.null(input$filter_files) && nzchar(input$filter_files)) {
        data <- data[data$files == input$filter_files, ]
      }
      
      data
    })
    
    output$last_update <- renderText({
      data <- process_data()
      req(data)
      
      last_time <- max(data$update_time, na.rm = TRUE)
      paste0("Данные обновлены: ", format(last_time, "%Y-%m-%d %H:%M:%S %Z"))
    })
    
    # Автообновление фильтров и автоустановка "файла"
    observe({
      all_data <- process_data()
      filtered_data <- processes_filtered_for_files()
      
      updateSelectInput(session, "filter_username", choices = sort(unique(all_data$username)), selected = isolate(input$filter_username))
      updateSelectInput(session, "filter_name", choices = sort(unique(all_data$name)), selected = isolate(input$filter_name))
      updateSelectInput(session, "filter_client", choices = sort(unique(all_data$client)), selected = isolate(input$filter_client))
      updateSelectInput(session, "filter_dir", choices = sort(unique(all_data$dir)), selected = isolate(input$filter_dir))
      
      # Обновление filter_files с автосменой selected
      files_choices <- sort(unique(filtered_data$files))
      selected_file <- isolate(input$filter_files)
      
      if (length(files_choices) > 0) {
        if (is.null(selected_file) || !(selected_file %in% files_choices)) {
          selected_file <- files_choices[1]
        }
      } else {
        selected_file <- ""
      }
      
      updateSelectInput(session, "filter_files", choices = files_choices, selected = selected_file)
    })
    
    output$process_table <- renderDT({
      req(process_data())
      datatable(
        filtered_processes() %>% select(-update_time),
        filter = "top",
        options = list(pageLength = 25, scrollX = TRUE),
        selection = 'none'
      )
    })
    
    observeEvent(input$kill_process, {
      req(input$filter_files)
      write_action_log(user = auth$user()$login, func = 'Process kill', session_id, value = input$filter_files)
      data <- process_data()
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

    })
  })
}


