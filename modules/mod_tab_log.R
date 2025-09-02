# ========================
# Модуль логов
# ========================

mod_tab_logs_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Логи",
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
            div(class = "card-footer", textOutput(ns("last_update")))
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
    # 👉 Новый блок с таблицей ошибок
    fluidRow(
      column(
        width = 12,
        div(class = "card",
            div(class = "card-header", "Лог ошибок приложения"),
            div(class = "card-body", DTOutput(ns("error_log_table")))
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


mod_tab_logs_server <- function(id, session_store, action_store, logs_last_update) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Загружаем данные
    sessions <- reactive({ session_store() })
    actions  <- reactive({ action_store() })
    
    # --- Лог ошибок (читаем один раз при старте) ---
    error_data <- get_error_log()
    output$error_log_table <- renderDT({
      datatable(
        error_data,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    # --- Фильтры ---
    output$user_filter <- renderUI({
      req(sessions())
      users <- unique(sessions()$user)
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
    
    # --- Фильтрованные данные ---
    filtered_sessions <- reactive({
      req(sessions())
      df <- sessions()
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
    
    # выбранная сессия
    selected_session_id <- reactive({
      if (is.null(input$sessions_table_rows_selected)) {
        return(NULL)
      }
      filtered_sessions()[input$sessions_table_rows_selected, "session_id", drop = TRUE]
    })
    
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
      # если выбрана конкретная сессия — фильтруем только её
      if (!is.null(selected_session_id())) {
        df <- df %>% filter(session_id == selected_session_id())
      }
      df
    })
    
    # --- Блок статистики ---
    output$stats_summary <- renderUI({
      req(filtered_sessions(), filtered_actions())
      users <- n_distinct(filtered_sessions()$user)
      sessions_count <- nrow(filtered_sessions())
      total_duration <- sum(filtered_sessions()$duration_seconds, na.rm = TRUE)
      actions_count <- nrow(filtered_actions())
      
      HTML(paste0(
        "<div>◦ Уникальных пользователей: ", users, "</div>",
        "<div>◦ Сессий: ", sessions_count, "</div>",
        "<div>◦ Общая длительность: ", format_seconds(total_duration), "</div>",
        "<div>◦ Событий: ", actions_count, "</div>"
      ))
    })
    
    # --- Таблицы ---
    output$sessions_table <- renderDT({
      datatable(
        filtered_sessions(),
        selection = "single",   # теперь можно выбрать одну строку
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    output$actions_table <- renderDT({
      datatable(filtered_actions(), options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # --- Новая таблица: Активность пользователей ---
    output$user_activity_table <- renderDT({
      req(filtered_sessions(), filtered_actions())
      activity <- filtered_sessions() %>%
        group_by(user) %>%
        summarise(
          sessions = n_distinct(session_id),
          actions = sum(session_id %in% filtered_actions()$session_id),
          total_duration = sum(duration_seconds, na.rm = TRUE),
          .groups = "drop"
        ) %>% 
        arrange(desc(sessions))
      datatable(activity, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # --- Новая таблица: Используемый функционал ---
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
      req(filtered_sessions())
      filtered_sessions() %>%
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
