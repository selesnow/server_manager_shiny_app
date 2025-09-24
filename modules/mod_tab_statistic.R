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
      column(
        width = 12,
        div(class = "card",
            div(class = "card-header", "Количество сбоев по задачам"),
            div(class = "card-body",
                dateRangeInput(ns("error_date_range"), "Дата", 
                               start = Sys.Date() - 30, end = Sys.Date()),
                DTOutput(ns("task_errors_table"))
            )
        )
      )
    ),
    fluidRow(
      column(6, 
             div(class = "card",
                 div(class = "card-header", "Активные задачи по годам"),
                 div(class = "card-body",
                     plotOutput(ns("task_log_plot")))
                 )
             ),
      column(6, 
             div(class = "card",
                 div(class = "card-header", "Активные задачи по годам"),
                 div(class = "card-body",
                     plotOutput(ns("task_info_plot")))
             )
             )
    ),
    fluidRow(
      column(
        width = 4,
        div(class = "card",
            div(class = "card-header", "Активные задачи по годам"),
            div(class = "card-body",
                DTOutput(ns("tasks_by_year_table"))
            )
        )
      ),
      column(
        width = 8,
        div(class = "card",
            div(class = "card-header", "График активных задач по годам"),
            div(class = "card-body",
                plotOutput(ns("tasks_by_year_plot"), height = "400px")
            )
        )
      )
    ),
    fluidRow(
      column(
        width = 4,
        div(class = "card",
            div(class = "card-header", "Запуски задач по часам"),
            div(class = "card-body",
                DTOutput(ns("tasks_by_hour_table"))
            )
        )
      ),
      column(
        width = 8,
        div(class = "card",
            div(class = "card-header", "График запусков задач по часам"),
            div(class = "card-body",
                plotOutput(ns("tasks_by_hour_plot"), height = "400px")
            )
        )
      )
    )
    
  )
}

# SERVER
mod_tab_statistic_server <- function(id, all_tasks, triggers, conf_rv) {
  moduleServer(id, function(input, output, session) {
    con <- dbConnect(SQLite(), conf_rv()$database_settings$task_log_base)
    
    # ---- Реактивы для существующей статистики ----
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
    
    # ---- Новый реактив для таблицы сбоев ----
    task_errors <- reactive({
      req(input$error_date_range)
      
      dbReadTable(con, "task_log") %>%
        mutate(log_time = str_sub(log_time, 1, 10) %>% as.Date()) %>%
        filter(log_time >= input$error_date_range[1],
               log_time <= input$error_date_range[2]) %>%
        summarise(errors = n_distinct(log_time), .by = task_name) %>%
        arrange(desc(errors))
    })
    
    output$task_errors_table <- renderDT({
      datatable(task_errors(),
                options = list(pageLength = 10, scrollX = TRUE),
                selection = "none")
    })
    
    # ---- Существующие UI-рендеры ----
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
    
    output$client_stats_table <- renderDT({
      datatable(client_stats(), options = list(pageLength = 10, scrollX = TRUE), selection = 'none')
    })
    output$author_stats_table <- renderDT({
      datatable(author_stats(), options = list(pageLength = 10, scrollX = TRUE), selection = 'none')
    })
    output$proj_elements_stats_table <- renderDT({
      datatable(proj_elements_stat(), options = list(pageLength = 10, scrollX = TRUE), selection = 'none')
    })
    
    # ---- Графики ----
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
        geom_rect(aes(xmin = 0, xmax = 0.5, ymin = ypos - 0.2, ymax = ypos + 0.2),
                  fill = "#ffcccc") +
        geom_rect(aes(xmin = 0.5, xmax = 0.8, ymin = ypos - 0.2, ymax = ypos + 0.2),
                  fill = "#fff2cc") +
        geom_rect(aes(xmin = 0.8, xmax = 1.0, ymin = ypos - 0.2, ymax = ypos + 0.2),
                  fill = "#d9ead3") +
        geom_rect(aes(xmin = 0, xmax = value, ymin = ypos - 0.1, ymax = ypos + 0.1),
                  fill = "steelblue") +
        geom_segment(aes(x = target, xend = target, y = ypos - 0.25, yend = ypos + 0.25),
                     color = "black", size = 1) +
        geom_text(aes(x = 1.05, label = scales::percent(value, accuracy = 1)),
                  hjust = 0, size = 4, color = "grey15") +
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
    
    output$task_log_plot <- renderPlot({
      dbReadTable(con, "task_log") %>% 
        mutate(log_time = str_sub(log_time, 1, 10) %>% as.Date()) %>% 
        summarise(failed_tasks = n_distinct(task_name), .by = log_time) %>% 
        filter(between(log_time, Sys.Date() - 90, Sys.Date())) %>% 
        ggplot(aes(x = log_time, y = failed_tasks)) + 
        geom_line() + geom_point() + 
        labs(title = 'Динамика сбоев планировщика заданий', x = '', y = '')
    })
    
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
    
    # ---- Новый реактив: количество задач по годам ----
    tasks_by_year <- reactive({
      req(all_tasks())
      
      all_tasks() %>% 
        filter(`Scheduled Task State` == "Enabled") %>% 
        mutate(`Start Year` = lubridate::year(`Start Date`)) %>% 
        filter(!is.na(`Start Year`)) %>% 
        summarise(
          tasks = dplyr::n_distinct(TaskName), .by = `Start Year`
        ) %>% 
        arrange(`Start Year`) %>% 
        mutate(rate = round(tasks / sum(tasks) * 100, 2))
    })
    
    # ---- Таблица ----
    output$tasks_by_year_table <- renderDT({
      datatable(tasks_by_year(),
                options = list(pageLength = 10, scrollX = TRUE),
                selection = "none")
    })
    
    # ---- График ----
    output$tasks_by_year_plot <- renderPlot({
      tasks_by_year() %>% 
        ggplot(aes(x = `Start Year`, y = tasks)) +
        geom_col(fill = "steelblue") +
        labs(
          title = "Количество активных задач по годам",
          x = "Год", y = "Количество задач"
        )
    })
    
    # ---- Новый реактив: количество запусков по часам (абс. + % от всех запусков) ----
    tasks_by_hour <- reactive({
      req(triggers(), all_tasks())
      
      df <- triggers() %>%
        # только активные таски
        filter(task_name %in% filter(all_tasks(), `Scheduled Task State` == "Enabled")$TaskName) %>%
        mutate(hour = format(start_boundary, "%H")) %>%
        summarise(task = dplyr::n(), .by = hour) %>%
        # дополняем отсутствующие часы 00-23
        right_join(tibble::tibble(hour = sprintf("%02d", 0:23)), by = "hour") %>%
        tidyr::replace_na(list(task = 0)) %>%
        arrange(hour)
      
      total <- sum(df$task)
      if (total == 0) {
        df <- df %>%
          mutate(
            pct = 0,
            pct_label = "0%"
          )
      } else {
        df <- df %>%
          mutate(
            pct = round(task / total * 100, 2),
            pct_label = scales::percent(task / total, accuracy = 0.1)
          )
      }
      
      df
    })
    
    # ---- Таблица по часам ----
    output$tasks_by_hour_table <- renderDT({
      datatable(
        tasks_by_hour() %>% select(hour, task, pct),
        colnames = c("Час", "Запуски (абс.)", "Доля (%)"),
        options = list(pageLength = 24, scrollX = TRUE),
        selection = "none"
      ) %>%
        DT::formatRound(columns = "pct", digits = 2)
    })
    
    # ---- График по часам (доля от всех запусков, градиент fill по доле) ----
    output$tasks_by_hour_plot <- renderPlot({
      df <- tasks_by_hour()
      
      # безопасная нормировка в 0..1
      total <- sum(df$task)
      df <- df %>% mutate(task_rel = if (total == 0) 0 else task / total)
      
      ggplot(df, aes(x = as.numeric(hour), y = task_rel, fill = task_rel)) +
        geom_col() +
        # подписи по оси X 0..23
        scale_x_continuous(breaks = 0:23) +
        # градиент: низ = синий, верх = красный
        scale_fill_gradient(
          low  = hcl(195, 100, 75),
          high = hcl(15, 100, 75),
          na.value = "grey90",
          labels = scales::percent_format(accuracy = 1)
        ) +
        # ось Y в процентах от общего числа запусков
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(
          title = "Количество запусков задач по часам",
          x = "Час суток",
          y = "Доля от всех запусков",
          fill = "Доля"
        ) + 
        theme(legend.position = "none")
    })
    
  })
}
