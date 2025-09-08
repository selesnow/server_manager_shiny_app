# ========================
# Модуль логов
# ========================

mod_tab_logs_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Логи",
    tags$link(rel = "stylesheet", type = "text/css", href = "css/calendar.css"),
    fluidRow(
      column(
        width = 12,
        div(class = "card",
            div(class = "card-header", "Фильтры"),
            div(class = "card-body",
                fluidRow(
                  column(3, dateRangeInput(ns("date_range"), "Дата", start = Sys.Date() - 30, end = Sys.Date())),
                  column(3, uiOutput(ns("user_filter"))),
                  column(3, uiOutput(ns("tab_filter"))),
                  column(3, uiOutput(ns("action_filter")))
                )
            ),
            div(class = "card-footer", style = "margin-top: 5px; font-size: 0.9em; color: #bbb;", textOutput(ns("last_update")))
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(class = "card",
            div(class = "card-header", "Общая статистика"),
            div(class = "card-body",
                uiOutput(ns("stats_summary"))
            )
        )
      )
    ),
    fluidRow(
      column(
        6,
        div(class = "card",
            div(class = "card-header", "Сессии"),
            div(class = "card-body", DTOutput(ns("sessions_table")))
        )
      ),
      column(
        6,
        div(class = "card",
            div(class = "card-header", "События"),
            div(class = "card-body", DTOutput(ns("actions_table")))
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(class = "card",
            div(class = "card-header", "Лог ошибок приложения"),
            div(class = "card-body", DTOutput(ns("error_log_table")))
        )
      )
    ),
    fluidRow(
      column(
        6,
        div(class = "card",
            div(class = "card-header", "Активность пользователей"),
            div(class = "card-body", DTOutput(ns("user_activity_table")))
        )
      ),
      column(
        6,
        div(class = "card",
            div(class = "card-header", "Используемый функционал"),
            div(class = "card-body", DTOutput(ns("function_usage_table")))
        )
      )
    ),
    # Графики
    fluidRow(
      column(6, plotOutput(ns("sessions_plot"))),
      column(6, plotOutput(ns("actions_plot")))
    )
  )
}

mod_tab_logs_server <- function(id, session_store, action_store, logs_last_update, conf_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Данные ---
    all_sessions <- reactive({ session_store() })
    actions      <- reactive({ action_store() })
    
    # --- Лог ошибок (читаем один раз при старте) ---
    error_data <- get_error_log()
    output$error_log_table <- renderDT({
      datatable(
        error_data,
        selection = "single",
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    # выбранный session_id из error_log
    selected_error_session_id <- reactive({
      if (is.null(input$error_log_table_rows_selected)) return(NULL)
      error_data[input$error_log_table_rows_selected, "session_id", drop = TRUE]
    })
    
    # --- Фильтры ---
    output$user_filter <- renderUI({
      req(all_sessions())
      users <- unique(all_sessions()$user)
      selectInput(ns("user"), "Пользователь", choices = c("Все", users), selected = "Все")
    })
    
    output$tab_filter <- renderUI({
      req(actions())
      tabs <- unique(actions()$tab)
      selectInput(ns("tab"), "Вкладка", choices = c("Все", tabs), selected = "Все")
    })
    
    output$action_filter <- renderUI({
      req(actions())
      acts <- unique(actions()$action)
      selectInput(ns("action"), "Действие", choices = c("Все", acts), selected = "Все")
    })
    
    # --- Базовая фильтрация сессий ---
    filtered_sessions_base <- reactive({
      req(all_sessions())
      df <- all_sessions()
      if (!is.null(input$date_range)) {
        df <- df %>% filter(date >= input$date_range[1], date <= input$date_range[2])
      }
      if (!is.null(input$user) && input$user != "Все") {
        df <- df %>% filter(user == input$user)
      }
      df
    })
    
    output$last_update <- renderText({
      req(logs_last_update())
      paste("Данные логов обновлены:", format(logs_last_update(), "%Y-%m-%d %H:%M:%S"))
    })
    
    # выбранная сессия из таблицы sessions
    selected_session_id_table <- reactive({
      if (is.null(input$sessions_table_rows_selected)) return(NULL)
      filtered_sessions_base()[input$sessions_table_rows_selected, "session_id", drop = TRUE]
    })
    
    # --- Итоговый выбранный session_id ---
    selected_session_id <- reactive({
      if (!is.null(selected_error_session_id())) {
        selected_error_session_id()
      } else {
        selected_session_id_table()
      }
    })
    
    # --- Финальные сессии ---
    filtered_sessions_final <- reactive({
      df <- filtered_sessions_base()
      # если выбран error_log → фильтруем только по нему
      if (!is.null(selected_error_session_id())) {
        df <- df %>% filter(session_id == selected_error_session_id())
      }
      df
    })
    
    # --- Фильтрованные действия ---
    filtered_actions <- reactive({
      req(actions())
      df <- actions()
      if (!is.null(input$date_range)) {
        df <- df %>% filter(as.Date(datetime) >= input$date_range[1], as.Date(datetime) <= input$date_range[2])
      }
      if (!is.null(input$user) && input$user != "Все") {
        df <- df %>% filter(user == input$user)
      }
      if (!is.null(input$tab) && input$tab != "Все") {
        df <- df %>% filter(tab == input$tab)
      }
      if (!is.null(input$action) && input$action != "Все") {
        df <- df %>% filter(action == input$action)
      }
      # всегда фильтруем по выбранному session_id (из таблицы или error_log)
      if (!is.null(selected_session_id())) {
        df <- df %>% filter(session_id == selected_session_id())
      }
      df
    })
    
    # --- Таблицы ---
    output$sessions_table <- renderDT({
      datatable(
        filtered_sessions_final(),
        selection = "single",
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    output$actions_table <- renderDT({
      datatable(filtered_actions(), options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # --- Активность пользователей ---
    output$user_activity_table <- renderDT({
      req(filtered_sessions_final(), filtered_actions())
      activity <- filtered_sessions_final() %>%
        group_by(user) %>%
        summarise(
          sessions = n_distinct(session_id),
          actions = sum(action_count, na.rm = T),
          total_duration = sum(duration_seconds, na.rm = TRUE),
          .groups = "drop"
        ) %>% 
        arrange(desc(sessions))
      datatable(activity, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # --- Используемый функционал ---
    output$function_usage_table <- renderDT({
      req(filtered_actions())
      usage <- filtered_actions() %>%
        group_by(action) %>%
        summarise(
          total_used = n(),
          sessions = n_distinct(session_id),
          .groups = "drop"
        ) %>% 
        arrange(desc(total_used))
      datatable(usage, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # --- Графики ---
    output$sessions_plot <- renderPlot({
      req(filtered_sessions_final())
      filtered_sessions_final() %>%
        count(date) %>%
        ggplot(aes(x = date, y = n, group = 1)) +
        geom_line() + 
        geom_point() +
        labs(title = "Количество сессий по дням", x = "", y = "")
    })
    
    output$actions_plot <- renderPlot({
      req(filtered_actions())
      filtered_actions() %>%
        mutate(date = as.Date(datetime)) %>%
        count(date) %>%
        ggplot(aes(x = date, y = n, group = 1)) +
        geom_line() +
        geom_point() +
        labs(title = "Количество событий по дням", x = "", y = "")
    })
  })
}


