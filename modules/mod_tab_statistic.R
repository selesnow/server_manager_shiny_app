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
      ),
    ),
    fluidRow(
      column(12, style = "height: 44.2vh;", plotOutput(ns("proj_elements_plot"), height = "100%"))
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
      column(
        width = 12,
        div(class = "card",
            div(class = "card-header", "Статистика по наличию элементов проекта"),
            div(class = "card-body",
                DTOutput(ns("proj_elements_stats_table"))
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
mod_tab_statistic_server <- function(id, all_tasks, conf_rv) {
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
    
    proj_elements_stat <- reactive({
      req(all_tasks())
      get_proj_elements_stat(all_tasks())
    })
    
    # Вывод общей статистики
    output$overall_stats_summary <- renderUI({
      stats <- overall_stats()
      HTML(paste0(
        "<div class='stats-item'>◦ Активных кронов: ", stats$total_crons, "</div>",
        "<div class='stats-item'>◦ Активных кронов в новой файловой структуре: ", 
        stats$new_structure_crons, " (", stats$new_structure_percent, " %)</div>",
        "<div class='stats-item'>◦ Активных кронов которые надо перенести: ", 
        stats$crons_to_move, " (", stats$to_move_percent, " %)</div>",
        "<div class='stats-item'>◦ Наличие элементов проектов: <br>",
        "<div style='margin-left: 30px;'>◦ README: ", stats$readme, " (",  stats$readme_rate, "%)</div>", 
        "<div style='margin-left: 30px;'>◦ NEWS: ", stats$news, " (",  stats$news_rate, "%)</div>", 
        "<div style='margin-left: 30px;'>◦ Git: ", stats$git, " (",  stats$git_rate, "%)</div>", 
        "<div style='margin-left: 30px;'>◦ Rproj: ",  stats$rproj, " (",  stats$rproj_rate, "%)</div>",
        "<div style='margin-left: 30px;'>◦ Logs: ",  stats$has_log, " (",  stats$has_log_rate, "%)</div></div>"
      ))
    })
    
    # Рендеринг таблиц статистики
    output$client_stats_table <- renderDT({
      datatable(client_stats(), options = list(pageLength = 10, scrollX = TRUE), selection = 'none')
    })
    
    output$author_stats_table <- renderDT({
      datatable(author_stats(), options = list(pageLength = 10, scrollX = TRUE), selection = 'none')
    })
    
    output$proj_elements_stats_table <- renderDT({
      datatable(proj_elements_stat(), options = list(pageLength = 10, scrollX = TRUE), selection = 'none')
    })
    
    # График наличия элементов проекта
    output$proj_elements_plot <- renderPlot(({
      
      indicators <- as.data.frame(overall_stats()) %>% 
        select(matches('rate|new_structure_percent')) %>% 
        pivot_longer(everything()) %>% 
        arrange(desc(value)) %>% 
        mutate(value = value / 100, target = 1, ypos = -row_number()) %>% 
        mutate(
          name = recode(
            name, 
            new_structure_percent = 'New Structure',
            has_log_rate          = 'Has Logs',
            rproj_rate            = 'Is Projects',
            readme_rate           = 'Has README',
            git_rate              = 'Use Git',
            news_rate             = 'Has NEWS'
          )
        )
      
      ggplot(indicators, aes(y = ypos)) +
        # фоновые зоны (например, красный-жёлтый-зелёный)
        geom_rect(aes(xmin = 0, xmax = 0.5, ymin = ypos - 0.2, ymax = ypos + 0.2),
                  fill = "#ffcccc") +
        geom_rect(aes(xmin = 0.5, xmax = 0.8, ymin = ypos - 0.2, ymax = ypos + 0.2),
                  fill = "#fff2cc") +
        geom_rect(aes(xmin = 0.8, xmax = 1.0, ymin = ypos - 0.2, ymax = ypos + 0.2),
                  fill = "#d9ead3") +
        # фактическое значение (тёмная полоса)
        geom_rect(aes(xmin = 0, xmax = value, ymin = ypos - 0.1, ymax = ypos + 0.1),
                  fill = "steelblue") +
        # цель (вертикальная линия)
        geom_segment(aes(x = target, xend = target, y = ypos - 0.25, yend = ypos + 0.25),
                     color = "black", size = 1) +
        # подпись справа
        geom_text(aes(x = 1.05, label = scales::percent(value, accuracy = 1)),
                  hjust = 0, size = 4) +
        scale_x_continuous(labels = scales::percent, limits = c(0, 1.1)) +
        scale_y_continuous(breaks = indicators$ypos, labels = indicators$name) +
        labs(x = NULL, y = NULL) +
        theme_minimal(base_size = 14) +
        theme(
          panel.grid  = element_blank(),
          axis.ticks  = element_blank(),
          axis.text.y = element_text(size = 16)
        ) + 
        labs(title = 'Наличие элементов проекта у скриптов')
      
    }))
    
    # Подключение к базе данных для графиков
    con <- dbConnect(SQLite(), conf_rv()$database_settings$task_log_base)
    
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
        filter(author != "Microsoft Visual Studio") %>%
        summarise(failed_tasks = n_distinct(task_name), .by = log_time) %>% 
        filter(between(log_time, Sys.Date() - 90, Sys.Date())) %>% 
        ggplot(aes(x = log_time, y = failed_tasks)) + 
        geom_line() + geom_point() + 
        labs(title = 'Динамика количества настроенных заданий в планировщике', x = '', y = '')
    })
    
  })
}
