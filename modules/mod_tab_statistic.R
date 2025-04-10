# UI модуля статистики
mod_tab_statistic_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Статистика",
    fluidRow(
      column(
        width = 12,
        div(class = "card",
            div(class = "card-header", "Общая статистика задач"),
            div(class = "card-body",
                div(class = "stats-summary",
                    uiOutput(ns("overall_stats_summary"))
                ),
                div(class = "stats-description",
                    h4("Показатели:"),
                    HTML("
                      <ul>
                        <li><strong>crons</strong> - К-во активных задач в планировщике заданий</li>
                        <li><strong>rate</strong> - Доля скриптов от общего активных количества по пользователю или клиенту</li>
                        <li><strong>new crons</strong> - К-во активных задач, скрипты которых перенесены в папку C:\\scripts</li>
                        <li><strong>new cron rate</strong> - Доля перенесённых в папку C:\\scripts от активных по пользователю</li>
                      </ul>
                    ")
                )
            )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(class = "card",
            div(class = "card-header", "Статистика по клиентам"),
            div(class = "card-body",
                DTOutput(ns("client_stats_table"))
            )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(class = "card",
            div(class = "card-header", "Статистика по авторам"),
            div(class = "card-body",
                DTOutput(ns("author_stats_table"))
            )
        )
      )
    ),
    fluidRow(
      column(6, plotOutput(ns("task_log_plot"))),
      column(6, plotOutput(ns("task_info_plot")))
    )
  )
}

# Серверная часть модуля статистики
mod_tab_statistic_server <- function(id, all_tasks) {
  moduleServer(id, function(input, output, session) {
    # Реактивные значения для статистики
    overall_stats <- reactive({
      req(all_tasks())
      get_overall_stats(all_tasks())
    })
    
    client_stats <- reactive({
      req(all_tasks())
      get_client_stats(all_tasks())
    })
    
    author_stats <- reactive({
      req(all_tasks())
      get_author_stats(all_tasks())
    })
    
    # Вывод общей статистики
    output$overall_stats_summary <- renderUI({
      stats <- overall_stats()
      HTML(paste0(
        "<div class='stats-item'>◦ Активных кронов: ", stats$total_crons, "</div>",
        "<div class='stats-item'>◦ Активных кронов в новой файловой структуре: ", 
        stats$new_structure_crons, " (", stats$new_structure_percent, " %)</div>",
        "<div class='stats-item'>◦ Активных кронов которые надо перенести: ", 
        stats$crons_to_move, " (", stats$to_move_percent, " %)</div>"
      ))
    })
    
    # Рендеринг таблиц статистики
    output$client_stats_table <- renderDT({
      datatable(client_stats(), options = list(pageLength = 10, scrollX = TRUE))
    })
    
    output$author_stats_table <- renderDT({
      datatable(author_stats(), options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # Подключение к базе данных для графиков
    con <- dbConnect(SQLite(), r"(C:\\scripts\\alsey\\netpeak_core\\nc_analytics_team\\telegram_bot\\bot_db.db)")
    
    # График динамики сбоев
    output$task_log_plot <- renderPlot({
      dbReadTable(con, "task_log") %>% 
        mutate(log_time = str_sub(log_time, 1, 10) %>% as.Date()) %>% 
        summarise(failed_tasks = n_distinct(task_name), .by = log_time) %>% 
        filter(between(log_time, Sys.Date() - 90, Sys.Date())) %>% 
        ggplot(aes(x = log_time, y = failed_tasks)) + 
        geom_line() + geom_point() + 
        labs(title = 'Динамика сбоев планировщика заданий', x = '', y = '')
    })
    
    # График количества заданий
    output$task_info_plot <- renderPlot({
      dbReadTable(con, "task_info_daily") %>% 
        mutate(log_time = str_sub(log_time, 1, 10) %>% as.Date()) %>% 
        summarise(failed_tasks = n_distinct(task_name), .by = log_time) %>% 
        filter(between(log_time, Sys.Date() - 90, Sys.Date())) %>% 
        ggplot(aes(x = log_time, y = failed_tasks)) + 
        geom_line() + geom_point() + 
        labs(title = 'Динамика количества настроенных заданий в планировщике', x = '', y = '')
    })

  })
}
