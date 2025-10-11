mod_tab_logs_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Логи",
    tabsetPanel(
      id = ns("logs_tabs"),
      
      # -----------------------
      # 1. Основные логи (текущий функционал)
      # -----------------------
      tabPanel(
        title = "Основные",
        tags$link(rel = "stylesheet", type = "text/css", href = "css/calendar.css"),
        
        fluidRow(
          column(
            width = 12,
            div(class = "card",
                div(class = "card-header", "Фильтры"),
                div(class = "card-body",
                    fluidRow(
                      column(2, dateRangeInput(ns("date_range"), "Дата", start = Sys.Date() - 30, end = Sys.Date())),
                      column(2, uiOutput(ns("user_filter"))),
                      column(2, uiOutput(ns("tab_filter"))),
                      column(2, uiOutput(ns("action_filter"))),
                      column(2, 
                             selectInput(ns("group_by"), "Группировка", 
                                         choices = c("День" = "day", "Неделя" = "week", "Месяц" = "month"), 
                                         selected = "day"))
                    ),
                    fluidRow(
                      column(12,
                             actionButton(ns("refresh_logs"), "Обновить логи", icon = icon("refresh"), class = "btn btn-primary")
                      )
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
          column(6, 
                 div(class = "card",
                     div(class = "card-header", "К-во сессий по дням"),
                     div(class = "card-body", plotOutput(ns("sessions_plot")))
                 )
          ),
          column(6, 
                 div(class = "card",
                     div(class = "card-header", "К-во событий по дням"),
                     div(class = "card-body", plotOutput(ns("actions_plot")))
                 )
          )
        ),
        fluidRow(
          column(6, 
                 div(class = "card",
                     div(class = "card-header", "К-во пользователей по дням"),
                     div(class = "card-body", plotOutput(ns("users_plot")))
                 )
          ),
          fluidRow(
            column(6, 
                   div(class = "card",
                       div(class = "card-header", "События по дням недели и времени суток"),
                       div(class = "card-body", plotOutput(ns("actions_heatmap"), height = "400px")))
            )
          ),
        ),
        fluidRow(
          column(12,
                 div(class = "card",
                     div(class = "card-header", "Статистика"),
                     div(class = "card-body", DTOutput(ns("stats_table")))
                 )
          )
        ),
        # --- Логи из app.Rout ---
        fluidRow(
          column(
            width = 12,
            div(class = "card",
                div(class = "card-header d-flex justify-content-between align-items-center",
                    "Содержимое app.Rout",
                    actionButton(ns("clear_rout"), "Очистить", class = "btn btn-danger btn-sm")
                ),
                div(
                  class = "card-body",
                  tags$div(
                    style = "background-color: #2a2a2a; color: #ddd; padding: 10px; border-radius: 5px; 
                         max-height: 600px; overflow-y: auto; font-family: monospace; white-space: pre-wrap;",
                    verbatimTextOutput(ns("task_log_content"))
                  )
                )
            )
          )
        )
        
      ),
      
      # -----------------------
      # 2. Логи AI чата
      # -----------------------
      tabPanel(
        title = "AI чат",
        fluidRow(
          column(
            width = 12,
            div(class = "card",
                div(class = "card-header", "Фильтры"),
                div(class = "card-body",
                    fluidRow(
                      column(3, dateRangeInput(ns("ai_date_range"), "Период", 
                                               start = Sys.Date() - 7, end = Sys.Date())),
                      column(3, uiOutput(ns("ai_user_filter"))),
                      column(2, 
                             actionButton(ns("ai_show_logs"), "Показать логи", 
                                          icon = icon("comments"), class = "btn btn-primary", width = "100%"))
                    )
                )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            div(class = "card",
                div(class = "card-header", "Диалоги с AI"),
                div(class = "card-body",
                    DTOutput(ns("ai_chat_log_table"))
                )
            )
          )
        )
      ),
      # -----------------------
      # 3. Логи поиска по файлам
      # -----------------------
      tabPanel(
        title = "Поиск по файлам",
        fluidRow(
          column(
            width = 12,
            div(class = "card",
                div(class = "card-header", "Фильтры"),
                div(class = "card-body",
                    fluidRow(
                      column(3, dateRangeInput(ns("files_date_range"), "Период", 
                                               start = Sys.Date() - 7, end = Sys.Date())),
                      column(3, uiOutput(ns("files_user_filter"))),
                      column(2, 
                             actionButton(ns("files_show_logs"), "Показать логи", 
                                          icon = icon("search"), class = "btn btn-primary", width = "100%"))
                    )
                )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            div(class = "card",
                div(class = "card-header", "Результаты поиска по файлам"),
                div(class = "card-body",
                    DTOutput(ns("files_log_table"))
                )
            )
          )
        )
      )
    )
  )
}

mod_tab_logs_server <- function(id, session_store, action_store, logs_last_update, conf_rv, auth, session_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Данные ---
    all_sessions <- reactive({ session_store() })
    actions      <- reactive({ action_store() })
    
    # --- РЕАКТИВКА для app.Rout ---
    rout_content <- reactiveVal("")
    
    read_rout <- function() {
      rout_path <- file.path(getwd(), "app.Rout")
      if (!file.exists(rout_path)) {
        return("Файл app.Rout не найден.")
      }
      paste(readLines(rout_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    }
    
    # читаем один раз при загрузке
    observe({
      rout_content(read_rout())
    })
    
    output$task_log_content <- renderText({
      rout_content()
    })
    
    
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
    
    # --- Функция агрегации по выбранной группировке ---
    aggregate_logs <- reactive({
      req(input$group_by, filtered_sessions_base(), filtered_actions())
      
      # полный диапазон дат
      min_date <- min(input$date_range)
      max_date <- max(input$date_range)
      
      period_seq <- switch(
        input$group_by,
        day   = seq(min_date, max_date, by = "day"),
        week  = seq(lubridate::floor_date(min_date, "week", week_start = 1),
                    lubridate::floor_date(max_date, "week", week_start = 1),
                    by = "week"),
        month = seq(lubridate::floor_date(min_date, "month"),
                    lubridate::floor_date(max_date, "month"),
                    by = "month")
      )
      
      # агрегируем сессии
      sessions_df <- filtered_sessions_base() %>%
        mutate(date = as.Date(date),
               period = case_when(
                 input$group_by == "day"   ~ date,
                 input$group_by == "week"  ~ lubridate::floor_date(date, "week", week_start = 1),
                 input$group_by == "month" ~ lubridate::floor_date(date, "month")
               )) %>%
        group_by(period) %>%
        summarise(
          sessions = n_distinct(session_id),
          users = n_distinct(user),
          .groups = "drop"
        ) %>%
        right_join(tibble(period = period_seq), by = "period") %>%
        mutate(
          sessions = replace_na(sessions, 0),
          users = replace_na(users, 0)
        )
      
      # агрегируем события
      actions_df <- filtered_actions() %>%
        mutate(date = as.Date(datetime),
               period = case_when(
                 input$group_by == "day"   ~ date,
                 input$group_by == "week"  ~ lubridate::floor_date(date, "week", week_start = 1),
                 input$group_by == "month" ~ lubridate::floor_date(date, "month")
               )) %>%
        group_by(period) %>%
        summarise(events = n(), .groups = "drop") %>%
        right_join(tibble(period = period_seq), by = "period") %>%
        mutate(events = replace_na(events, 0))
      
      # объединяем
      full_join(sessions_df, actions_df, by = "period")
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
      df <- aggregate_logs()
      ggplot(df, aes(x = period, y = sessions, group = 1)) +
        geom_line() + geom_point() +
        labs(title = "Количество сессий", x = "", y = "")
    })
    
    output$actions_plot <- renderPlot({
      df <- aggregate_logs()
      ggplot(df, aes(x = period, y = events, group = 1)) +
        geom_line() + geom_point() +
        labs(title = "Количество событий", x = "", y = "")
    })
    
    output$users_plot <- renderPlot({
      df <- aggregate_logs()
      ggplot(df, aes(x = period, y = users, group = 1)) +
        geom_line(color = "steelblue") +
        geom_point(color = "steelblue") +
        labs(title = "Уникальные пользователи", x = "", y = "")
    })
    
    output$actions_heatmap <- renderPlot({
      req(filtered_actions())
      
      filtered_actions() %>%
        mutate(
          wday = lubridate::wday(datetime, week_start = 1),   # Пн=1 … Вс=7
          wday = factor(
            wday,
            levels = 1:7,
            labels = c("Пн","Вт","Ср","Чт","Пт","Сб","Вс"),
            ordered = TRUE
          ),
          hour = lubridate::hour(datetime)
        ) %>%
        count(wday, hour) %>%
        ggplot(aes(x = hour, y = fct_rev(wday), fill = n)) +
        geom_tile(color = "white") +
        scale_fill_gradient(
          low = hcl(195, 100, 75),   # синий
          high = hcl(15, 100, 75),   # красный
          na.value = "grey90"
        ) +
        scale_x_continuous(breaks = 0:23) +
        labs(
          title = "Количество действий по дням недели и часам суток",
          x = "Час суток",
          y = "День недели",
          fill = "Кол-во"
        ) +
        theme(
          panel.grid = element_blank(),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          legend.position = "none"
        )
    })
    
    output$stats_table <- renderDT({
      df <- aggregate_logs()
      datatable(df, options = list(pageLength = 15, scrollX = TRUE))
    })
    
    
    # очистка app.Rout
    observeEvent(input$clear_rout, {
      
      write_action_log(user = auth$user()$login, func = 'Cleare app.Rout', session_id)
      rout_path <- file.path(getwd(), "app.Rout")
      if (file.exists(rout_path)) {
        # очищаем файл
        write("", file = rout_path)
      }
      
      # обновляем reactiveVal, чтобы сразу отобразилось пустое поле
      rout_content("")
      
      showNotification("Файл app.Rout очищен", type = "message")
    })
    
    # обновление логов
    observeEvent(input$refresh_logs, {
      # Обновляем "метку обновления"
      logs_last_update(lubridate::with_tz(Sys.time(), "Europe/Kyiv"))
      
      # Если у тебя есть функции обновления сессий и экшенов, дерни их тут
      # например:
      session_store(get_session_log())
      action_store(get_action_log())
      rout_content(read_rout())
      
      showNotification("Логи обновлены", type = "message")
    })
    
    # =========================================================
    # Вкладка AI чат — исправленная версия (использует app_con)
    # =========================================================
    
    # UI: остаётся как у тебя — только серверная логика ниже
    
    # --- UI фильтр пользователей ---
    output$ai_user_filter <- renderUI({
      users <- tryCatch({
        con <- dbConnect(SQLite(), conf_rv()$database_settings$app_data_base)
        df <- DBI::dbGetQuery(con, "SELECT DISTINCT user FROM ai_chat_log ORDER BY user")
        dbDisconnect(con)
        if (nrow(df) == 0) character(0) else df$user
      }, error = function(e) {
        warning("Не удалось получить список пользователей из ai_chat_log: ", conditionMessage(e))
        character(0)
      })
      
      selectInput(ns("ai_user"), "Пользователь", choices = c("Все", users), selected = "Все")
    })
    
    # --- Реактивка для логов: загружаем только по нажатию ---
    ai_chat_data <- eventReactive(input$ai_show_logs, {
      req(input$ai_date_range)
      req(input$ai_user)
      start <- as.character(input$ai_date_range[1])
      end   <- as.character(input$ai_date_range[2])
      
      # безопасно формируем SQL: датами напрямую, user — через dbQuoteString
      base_sql <- sprintf(
        "SELECT id, datetime, session_id, user, role, message FROM ai_chat_log
     WHERE date(datetime) BETWEEN '%s' AND '%s'",
        start, end
      )
      
      if (!is.null(input$ai_user) && input$ai_user != "Все") {
        con <- dbConnect(SQLite(), conf_rv()$database_settings$app_data_base)
        user_q <- DBI::dbQuoteString(con, as.character(input$ai_user))
        dbDisconnect(con)
        base_sql <- paste0(base_sql, " AND user = ", user_q)
      }
      
      df <- tryCatch({
        con <- dbConnect(SQLite(), conf_rv()$database_settings$app_data_base)
        ai_logs <- DBI::dbGetQuery(con, base_sql)
        dbDisconnect(con)
        ai_logs
      }, error = function(e) {
        showNotification(paste("Ошибка при чтении ai_chat_log:", conditionMessage(e)), type = "error")
        tibble::tibble()
      })
      
      # форматируем datetime и конвертим markdown→HTML (если хочешь)
      if (nrow(df) > 0) {
        df$datetime <- format(as.POSIXct(df$datetime), "%Y-%m-%d %H:%M:%S")
        # если хочешь рендерить markdown в HTML — раскомментируй и подключи пакет commonmark
        df$message <- vapply(df$message, function(x) commonmark::markdown_html(as.character(x)), character(1))
      }
      
      df
    })
    
    # --- Таблица для отображения ---
    output$ai_chat_log_table <- renderDT({
      df <- ai_chat_data()
      req(df)
      
      datatable(
        df,
        escape = FALSE,           # нужно, если ты конвертируешь markdown → HTML выше
        options = list(pageLength = 20, scrollX = TRUE),
        rownames = FALSE
      )
    })
    
    # =========================================================
    # Вкладка Поиск по файлам
    # =========================================================
    
    # --- UI фильтр пользователей ---
    output$files_user_filter <- renderUI({
      users <- tryCatch({
        con <- dbConnect(SQLite(), conf_rv()$database_settings$app_data_base)
        df <- DBI::dbGetQuery(con, "SELECT DISTINCT user FROM find_in_files_log ORDER BY user")
        dbDisconnect(con)
        if (nrow(df) == 0) character(0) else df$user
      }, error = function(e) {
        warning("Не удалось получить список пользователей из find_in_files_log: ", conditionMessage(e))
        character(0)
      })
      
      selectInput(ns("files_user"), "Пользователь", choices = c("Все", users), selected = "Все")
    })
    
    # --- Реактивка: загружаем логи только по нажатию кнопки ---
    files_log_data <- eventReactive(input$files_show_logs, {
      req(input$files_date_range)
      req(input$files_user)
      
      start <- as.character(input$files_date_range[1])
      end   <- as.character(input$files_date_range[2])
      
      base_sql <- sprintf(
        "SELECT * 
     FROM find_in_files_log
     WHERE date(datetime) BETWEEN '%s' AND '%s'",
        start, end
      )
      
      if (!is.null(input$files_user) && input$files_user != "Все") {
        con <- dbConnect(SQLite(), conf_rv()$database_settings$app_data_base)
        user_q <- DBI::dbQuoteString(con, as.character(input$files_user))
        dbDisconnect(con)
        base_sql <- paste0(base_sql, " AND user = ", user_q)
      }
      
      df <- tryCatch({
        con <- dbConnect(SQLite(), conf_rv()$database_settings$app_data_base)
        files_logs <- DBI::dbGetQuery(con, base_sql)
        dbDisconnect(con)
        files_logs
      }, error = function(e) {
        showNotification(paste("Ошибка при чтении find_in_files_log:", conditionMessage(e)), type = "error")
        tibble::tibble()
      })
      
      if (nrow(df) > 0) {
        df$datetime <- format(as.POSIXct(df$datetime), "%Y-%m-%d %H:%M:%S")
      }
      
      df
    })
    
    # --- Таблица с результатами поиска ---
    output$files_log_table <- renderDT({
      df <- files_log_data()
      req(df)
      
      datatable(
        df,
        options = list(pageLength = 20, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
}
