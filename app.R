library(shiny)
library(shinyjs)
library(shinychat)
library(DT)
library(dplyr)
library(taskscheduleR)
library(stringr)
library(readr)
library(glue)
library(snakecase)
library(RSQLite)
library(ggplot2)
library(findInFiles)
library(purrr)

# Загрузка вспомогательных функций
for(fun in dir(here::here("R"))) source(here::here("R", fun))

# Загрузка модуля авторизации
source("modules/mod_auth.R")
# Загрузка модуля интерфейса управления пользователями
source("modules/mod_tab_access.R")
# Загрузка модуля CMD
source("modules/mod_tab_cmd.R")
# Загрузка модуля Служб
source("modules/mod_tab_services.R")

# Генерация интерфейса ----------------------------------------------------
ui <- fluidPage(
  mod_auth_ui("auth"),     # Модуль авторизации
  uiOutput("app_ui")       # Основной контент
)


# Серверная часть ---------------------------------------------------------
server <- function(input, output, session) {

  # Проверка авторизации ----------------------------------------------------
  # Подключение к базе данных SQLite
  # Коннект к БД
  app_con <- dbConnect(RSQLite::SQLite(), "app.db")
  
  observe({
    if (logged_in() && user_role() == "admin") {
      mod_access_server("access", conn = app_con)
    }
  })
  
  
  check_user <- function(login, password) {
    query <- paste("SELECT * FROM users WHERE login = '", login, "' AND password = '", password, "'", sep = "")
    res <- dbGetQuery(app_con, query)
    if (nrow(res) > 0) {
      return(res)
    } else {
      return(NULL)
    }
  }
  
  # Reactives
  logged_in <- reactiveVal(FALSE)
  user_role <- reactiveVal(NULL)
  
  # Модуль авторизации
  mod_auth_server("auth", logged_in, user_role, check_user_fun = check_user)

  # UI для основного контента
  output$app_ui <- renderUI({
    if (logged_in()) {
      
      # Загрузка основного интерфейса
      fluidPage(
        useShinyjs(),  # Добавляем использование shinyjs
        
        # Добавляем возможность переключения темной темы
        tags$head(
          tags$head(
            # Добавляем иконку для вкладки браузера
            tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
            tags$style(HTML("
              .header-container {
                  display: flex;
                  justify-content: flex-start;  /* Расположить элементы слева */
                  align-items: center;          /* Выравнивание по вертикали */
                  padding: 10px 20px;           /* Отступы вокруг */
                  background-color: #333;       /* Темно-серый фон */
                  color: #fff;                  /* Белый цвет текста */
                  border-radius: 5px;           /* Скругление углов */
                  margin-bottom: 10px;          /* Отступ снизу */
                  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);  /* Тень */
              }
              
              .controls-wrapper {
                  display: flex;
                  justify-content: flex-start;  /* Выравнивание контейнера слева */
                  align-items: center;
              }
              
              .controls-container {
                  display: flex;
                  gap: 10px;  /* Расстояние между элементами */
                  align-items: center;  /* Выравнивание по вертикали */
              }
              
              .controls-container .btn-sm {
                  padding: 5px 10px;  /* Немного уменьшаем размер кнопки */
              }
              
              /* Стили для светлой темы, если нужно */
              .light-mode .header-container {
                  background-color: #f7f7f7; /* Светлый фон, если активирована светлая тема */
                  color: #333;  /* Тёмный текст */
              }
          "))
          ),
          tags$style(HTML("
            body {
              background-color: #333;
              color: #f5f5f5;
            }
            .card {
              background-color: #444;
              border-color: #555;
              margin-bottom: 20px;
            }
            .card-header {
              background-color: #555;
              border-color: #666;
            }
            .btn-primary {
              background-color: #007bff;
            }
            .btn-success {
              background-color: #28a745;
            }
            .btn-danger {
              background-color: #dc3545;
            }
            .btn-warning {
              background-color: #ffc107;
              color: #333;
            }
            .btn-info {
              background-color: #17a2b8;
            }
            select, input {
              background-color: #555;
              color: #f5f5f5;
              border-color: #666;
            }
            table {
              color: #f5f5f5;
            }
            .dataTables_wrapper {
              color: #f5f5f5;
            }
            .dataTables_info, 
            .dataTables_paginate,
            .dataTables_filter,
            .dataTables_length {
              color: #f5f5f5 !important;
            }
            
            /* Стили для светлой темы */
            .light-mode {
              background-color: #ffffff;
              color: #333;
            }
            .light-mode .card {
              background-color: #f9f9f9;
              border-color: #ddd;
            }
            .light-mode .card-header {
              background-color: #f1f1f1;
              border-color: #ddd;
            }
            .light-mode select, .light-mode input {
              background-color: #ffffff;
              color: #333;
              border-color: #ccc;
            }
            .light-mode table {
              color: #333;
            }
            .light-mode .dataTables_wrapper {
              color: #333;
            }
            .light-mode .dataTables_info, 
            .light-mode .dataTables_paginate,
            .light-mode .dataTables_filter,
            .light-mode .dataTables_length {
              color: #333 !important;
            }
            
            /* Стиль для светлой темы логов */
            .light-mode .light-mode-log {
              background-color: #f0f0f0 !important;
              color: #333 !important;
            }
            
            /* Стиль для header */
            .header-container {
              display: flex;
              justify-content: space-between;
              align-items: center;
            }
            .controls-container {
              display: flex;
              align-items: center;
            }
            
            /* Стиль для вкладок */
            .nav-tabs {
              border-bottom: 1px solid #555;
            }
            .nav-tabs .nav-link {
              color: #f5f5f5;
              background-color: #444;
              border-color: #555;
            }
            .nav-tabs .nav-link.active {
              color: #f5f5f5;
              background-color: #555;
              border-color: #666;
            }
            .light-mode .nav-tabs .nav-link {
              color: #333;
              background-color: #f1f1f1;
              border-color: #ddd;
            }
            .light-mode .nav-tabs .nav-link.active {
              color: #333;
              background-color: #ffffff;
              border-color: #ddd;
            }
            
            /* Стили для статистики */
            .stats-summary {
              margin-bottom: 20px;
              padding: 15px;
              border-radius: 5px;
              background-color: #555;
            }
            .light-mode .stats-summary {
              background-color: #f1f1f1;
            }
            .stats-item {
              margin-bottom: 8px;
            }
            .stats-description {
              margin-top: 15px;
              padding: 15px;
              border-radius: 5px;
              background-color: #555;
            }
            .light-mode .stats-description {
              background-color: #f1f1f1;
            }
            .stats-description h4 {
              margin-bottom: 10px;
            }
          "))
        ),
        
        # Используем правильную структуру заголовка без встроенных стилей
        titlePanel(
          title = "Server Task & Service Manager",
          windowTitle = "Server Task & Service Manager"
        ),
        
        # Добавляем элементы управления отдельно, после заголовка
        div(class = "header-container", 
            div(class = "controls-wrapper", 
                div(class = "controls-container", 
                    actionButton("refresh_data", "Обновить данные", icon = icon("refresh"), class = "btn-sm ml-2"),
                    checkboxInput("dark_mode", "Светлая тема", FALSE)
                )
            )
        ),
        
        # Начало вкладок
        tabsetPanel(
          id = "main_tabs",
          
          # Вкладка "Задачи"
          tabPanel(
            title = "Задачи",
            
            # Первый вертикальный блок - фильтры и управление задачами
            fluidRow(
              column(
                width = 12,
                div(class = "card", 
                    div(class = "card-header", "Фильтры и управление задачами"),
                    div(class = "card-body",
                        fluidRow(
                          # Блок с фильтрами
                          column(
                            width = 2,
                            div(class = "mb-3", 
                                h4("Фильтры задач"),
                                uiOutput("author_filter"),
                                uiOutput("runas_filter"),
                                uiOutput("last_result_filter"),
                                uiOutput("client_filter") # Добавляем фильтр по клиенту
                            )
                          ),
                          # Блок с управлением задачами
                          column(
                            width = 5,
                            div(class = "mb-3",
                                h4("Управление задачами"),
                                selectInput("selected_task", "Выберите задачу:", choices = NULL, width = '750px'),
                                div(class = "action-buttons",
                                    actionButton("run_task", "Запустить", icon = icon("play"), class = "btn-success"),
                                    actionButton("view_task_logs", "Логи", icon = icon("file-alt"), class = "btn-info"),
                                    actionButton("view_task_readme", "README", icon = icon("file-alt"), class = "btn-info")
                                ),
                                # Добавляем блок информации о задаче
                                div(class = "card mt-3", id = "task_info_card",
                                    div(class = "card-header", "Информация о задаче"),
                                    div(class = "card-body",
                                        uiOutput("selected_task_info")
                                    )
                                )
                            )
                          )
                        )
                    )
                )
              )
            ),
            
            # Блок с выводом лога
            fluidRow(
              column(
                width = 12,
                div(class = "card", style = "display: none;", id = "log_card",
                    div(class = "card-header", "Логи задачи"),
                    div(class = "card-body",
                        h4(textOutput("log_task_name")),
                        tags$div(
                          style = "background-color: #2a2a2a; color: #ddd; padding: 10px; border-radius: 5px; max-height: 400px; overflow-y: auto;",
                          class = "light-mode-log",
                          verbatimTextOutput("task_log_content")
                        )
                    )
                )
              )
            ),
            
            # Блок с выводом лога
            fluidRow(
              column(
                width = 12,
                div(class = "card", style = "display: none;", id = "readme_card",
                    div(class = "card-header", "README"),
                    div(class = "card-body",
                        h4(textOutput("readme_task_name")),
                        tags$div(
                          style = "background-color: #2a2a2a; color: #ddd; padding: 10px; border-radius: 5px; max-height: 400px; overflow-y: auto;",
                          class = "light-mode-log",
                          uiOutput("task_readme_content")
                        )
                    )
                )
              )
            ),
            
            # Таблица задач
            fluidRow(
              column(
                width = 12,
                div(class = "card",
                    div(class = "card-header", "Задачи"),
                    div(class = "card-body",
                        DTOutput("task_table")
                    )
                )
              )
            )
          ),
          
          # Вкладка "Службы"
          mod_tab_services_ui("services_tab"),
          
          # Улучшенная вкладка "Статистика"
          tabPanel(
            title = "Статистика",
            
            # Добавляем блок с общей статистикой
            fluidRow(
              column(
                width = 12,
                div(class = "card",
                    div(class = "card-header", "Общая статистика задач"),
                    div(class = "card-body",
                        # Секция с общими показателями
                        div(class = "stats-summary",
                            uiOutput("overall_stats_summary")
                        ),
                        
                        # Секция с описанием показателей
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
            
            # Таблица статистики по клиентам
            fluidRow(
              column(
                width = 12,
                div(class = "card",
                    div(class = "card-header", "Статистика по клиентам"),
                    div(class = "card-body",
                        DTOutput("client_stats_table")
                    )
                )
              )
            ),
            
            # Таблица статистики по авторам
            fluidRow(
              column(
                width = 12,
                div(class = "card",
                    div(class = "card-header", "Статистика по авторам"),
                    div(class = "card-body",
                        DTOutput("author_stats_table")
                    )
                )
              )
            ),
            # графики
            fluidRow(
              column(6, plotOutput("task_log_plot")),
              column(6, plotOutput("task_info_plot"))
            ),
            hr(),
            fluidRow(
              column(12,
                     DTOutput("statistics_table")
              )
            )
          ),
          
          # Вкладка "CMD"
          mod_tab_cmd_ui("cmd"),
          
          # Поиск по файлам
          tabPanel(
            title = "Поиск по файлам",
            fluidRow(
              column(
                width = 12,
                div(class = "card", 
                    div(class = "card-header", "Поиск по файлам в директориях"),
                    div(class = "card-body",
                        div(
                          h4("Введите текст для поиска"),
                          uiOutput("file_search_ui"),
                          fluidRow(
                            column(10,
                                   textInput("file_pattern", NULL, placeholder = "Введите строку для поиска...")),
                            column(2,
                                   actionButton("search_btn", "Найти", class = "btn-primary"))
                          ),
                          hr(),
                          DT::dataTableOutput("search_results"),
                          uiOutput("search_message")
                        )
                    )
                )
              )
            )
          ),
          if (user_role() == "admin") {
            tabPanel("Доступ", mod_access_ui("access"))
          }
        ),
        
        # Добавляем CSS для кнопок действий
        tags$head(
          tags$style(HTML("
            .action-buttons {
              display: flex;
              gap: 10px;
              flex-wrap: wrap;
              margin-top: 10px;
            }
          "))
        ),
        actionButton("logout_btn", "Выйти"),
        
        # JavaScript для переключения темной/светлой темы
        tags$script(HTML("
          $(document).ready(function() {
            // Темная тема по умолчанию
            $('#dark_mode').on('change', function() {
              if($(this).is(':checked')) {
                $('body').addClass('light-mode');
              } else {
                $('body').removeClass('light-mode');
              }
            });
          });
        "))
      )
    }
  })
  
  # Обработчик кнопки "Войти"
  observeEvent(input$login_btn, {
    user <- check_user(input$login, input$password)
    
    if (!is.null(user)) {
      logged_in(TRUE)  # Успешная авторизация
      user_role(user$role)  # Сохраняем роль пользователя
    } else {
      output$login_message <- renderText("Неверный логин или пароль")
    }
  })
  
  # Обработчик кнопки "Выйти"
  observeEvent(input$logout_btn, {
    logged_in(FALSE)
    user_role(NULL)
  })
  
  # Инфо по задачам
  all_tasks <- reactiveVal(NULL)
  
  # Добавляем реактивное значение для отслеживания обновлений
  refresh_trigger <- reactiveVal(0)
  
  # Модифицируем реактивное значение services_data, чтобы оно зависело от refresh_trigger
  services_data <- reactive({
    # Это заставит services_data пересчитываться каждый раз при изменении refresh_trigger
    refresh_trigger()
    get_services()
  })
  
  # Основная логика приложения, запускается после логина
  observeEvent(logged_in(), {
    if (logged_in()) {
      
      observe({
        all_tasks(get_tasks())
      })
      
      task_data <- reactive({
        req(all_tasks())
        task <- all_tasks()
        
        if (!is.null(input$filter_author)) {
          task <- task %>% filter(Author %in% input$filter_author)
        }
        if (!is.null(input$filter_runas)) {
          task <- task %>% filter(`Run As User` %in% input$filter_runas)
        }
        if (!is.null(input$filter_last_result)) {
          task <- task %>% filter(`Last Result` %in% input$filter_last_result)
        }
        if (!is.null(input$filter_client)) {
          task <- task %>% filter(Client %in% input$filter_client)
        }
        
        task
      })
      
      # модуль служб ------------------------------------------------------------
      mod_tab_services_server("services_tab", services_data)
      
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
      
      # Выводим общую статистику
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
      
      filtered_task_names <- reactive({
        task_data()$TaskName
      })
      
      observe({
        updateSelectInput(session, "selected_task", choices = filtered_task_names())
      })
      
      output$author_filter <- renderUI({
        req(all_tasks())
        selectInput("filter_author", "Author:", choices = unique(all_tasks()$Author), multiple = TRUE)
      })
      
      output$runas_filter <- renderUI({
        req(all_tasks())
        selectInput("filter_runas", "Run As User:", choices = unique(all_tasks()$`Run As User`), multiple = TRUE)
      })
      
      output$last_result_filter <- renderUI({
        req(all_tasks())
        selectInput("filter_last_result", "Last Result:", choices = unique(all_tasks()$`Last Result`), multiple = TRUE)
      })
      
      # Добавляем фильтр по клиенту
      output$client_filter <- renderUI({
        req(all_tasks())
        selectInput("filter_client", "Client:", choices = unique(all_tasks()$Client), multiple = TRUE)
      })
      
      output$task_table <- renderDT({
        datatable(task_data(), filter = "top", options = list(pageLength = 10, scrollX = TRUE))
      })
      
      # Добавим обработчик для поиска в таблице задач, если он нужен
      filtered_task_data <- reactive({
        data <- task_data()
        
        if (!is.null(input$task_search) && input$task_search != "") {
          search_term <- tolower(input$task_search)
          data <- data[apply(data, 1, function(row) any(grepl(search_term, tolower(row), fixed = TRUE))), ]
        }
        
        return(data)
      })
      
      # Заменим обработчик таблицы задач, чтобы использовать фильтрацию
      output$task_table <- renderDT({
        datatable(filtered_task_data(), filter = "top", options = list(pageLength = 10, scrollX = TRUE))
      })
      
      # Добавим обработчик для поиска в таблице служб
      filtered_service_data <- reactive({
        data <- services_data()
        
        if (!is.null(input$service_search) && input$service_search != "") {
          search_term <- tolower(input$service_search)
          data <- data[apply(data, 1, function(row) any(grepl(search_term, tolower(row), fixed = TRUE))), ]
        }
        
        return(data)
      })
      
      # Заменим обработчик таблицы служб, чтобы использовать фильтрацию
      output$service_table <- renderDT({
        datatable(filtered_service_data(), options = list(pageLength = 5))
      })
      
      # Графики
      con <- dbConnect(SQLite(), r"(C:\\scripts\\alsey\\netpeak_core\\nc_analytics_team\\telegram_bot\\bot_db.db)")
      
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
          summarise(failed_tasks = n_distinct(task_name), .by = log_time) %>% 
          filter(between(log_time, Sys.Date() - 90, Sys.Date())) %>% 
          ggplot(aes(x = log_time, y = failed_tasks)) + 
          geom_line() + geom_point() + 
          labs(title = 'Динамика количества настроенных заданий в планировщике', x = '', y = '')
      })
    }
  }
  )
  
  # Модифицируем обработчик для кнопки обновления данных
  observeEvent(input$refresh_data, {
    # Обновляем данные о задачах
    all_tasks(get_tasks())
    # Обновление данных о службах
    services_data(get_services())
    
    # Увеличиваем значение refresh_trigger, что вызовет перерасчет services_data()
    refresh_trigger(refresh_trigger() + 1)
    
    # Показываем уведомление об успешном обновлении
    showNotification("Данные успешно обновлены", type = "message", duration = 3)
  })
  
  # Запуск задачи
  observeEvent(input$run_task, {
    req(input$selected_task)
    taskscheduler_runnow(taskname = input$selected_task)
    showNotification(str_glue("Задача '{input$selected_task}' запущена."), type = "message")
  })
  
  # Поиск и чтение лога
  observeEvent(input$view_task_logs, {
    req(input$selected_task)
    
    # Находим выбранную задачу в таблице
    selected_task_data <- all_tasks() %>% 
      filter(TaskName == input$selected_task)
    
    if(nrow(selected_task_data) > 0) {
      # Получаем нужные поля
      task_to_run <- selected_task_data$`Task To Run`
      start_in <- selected_task_data$`Start In`
      
      # Если данные получены, вызываем функцию find_log (с чтением последних 25 строк)
      if(!is.null(task_to_run) && !is.null(start_in)) {
        log_content <- try(find_log(task_to_run = task_to_run, start_in = start_in), silent = TRUE)
        
        if(inherits(log_content, "try-error")) {
          output$task_log_content <- renderText({ "Ошибка при чтении лога" })
        } else {
          output$task_log_content <- renderText({ log_content })
        }
        
        # Показываем имя задачи
        output$log_task_name <- renderText({ paste("Лог задачи:", input$selected_task) })
        
        # Показываем блок с логами
        shinyjs::show(id = "log_card")
      } else {
        showNotification("Не удалось найти данные для задачи", type = "error")
      }
    } else {
      showNotification("Задача не найдена", type = "error")
    }
  })
  
  # Поиск и чтение README
  observeEvent(input$view_task_readme, {
    req(input$selected_task)
    
    # Находим выбранную задачу в таблице
    selected_task_data <- all_tasks() %>% 
      filter(TaskName == input$selected_task)
    
    if(nrow(selected_task_data) > 0) {
      # Получаем нужные поля
      task_to_run <- selected_task_data$`Task To Run`
      start_in <- selected_task_data$`Start In`
      
      # Если данные получены, вызываем функцию find_log (с чтением последних 25 строк)
      if( !is.null(start_in)) {
        readme_content <- try(find_readme(start_in = start_in), silent = TRUE)
        
        if(inherits(readme_content, "try-error")) {
          output$task_readme_content <- renderUI({ HTML("<p>Ошибка при чтении README</p>") })
        } else {
          output$task_readme_content <- renderUI({ HTML(readme_content) })
        }
        
        # Показываем имя задачи
        output$readme_task_name <- renderText({ paste("README:", input$selected_task) })
        
        # Показываем блок с README
        shinyjs::show(id = "readme_card")
        
      } else {
        showNotification("Не удалось найти данные для задачи", type = "error")
      }
    } else {
      showNotification("Задача не найдена", type = "error")
    }
  })
  

  # Вывод дополнительный информации о выбранной задаче ----------------------
  selected_task_details <- reactive({
    req(input$selected_task)
    all_tasks() %>% 
      filter(TaskName == input$selected_task) %>%
      select(TaskName, Author, `Run As User`, `Start In`, `Task To Run`, Client, Comment, `Last Run Time`, `Last Result`)
  })
  
  # Рендерим информацию о выбранной задаче
  output$selected_task_info <- renderUI({
    req(selected_task_details())
    task_info <- selected_task_details()
    
    if(nrow(task_info) > 0) {
      task <- task_info[1, ]
      div(
        div(class = "mb-2", strong("Автор: "), span(task$Author)),
        div(class = "mb-2", strong("Запускается от имени: "), span(task$`Run As User`)),
        div(class = "mb-2", strong("Директория: "), span(task$`Start In`)),
        div(class = "mb-2", strong("Команда запуска: "), span(task$`Task To Run`)),
        div(class = "mb-2", strong("Время прошлого запуска: "), span(task$`Last Run Time`)),
        div(class = "mb-2", strong("Результат прошлого запуска: "), span(task$`Last Result`)),
        div(class = "mb-2", strong("Клиент: "), span(task$Client)),
        div(class = "mb-2", strong("Краткое описание: "), span(task$Comment))
      )
    } else {
      div("Информация недоступна")
    }
  })
  
  # Показываем информацию при выборе задачи
  observeEvent(input$selected_task, {
    shinyjs::show(id = "task_info_card")
  })
  
  # Командная строка --------------------------------------------------------
  mod_tab_cmd_server("cmd")
  
  # Поиск по файлам ---------------------------------------------------------
  # Заранее определённые директории
  search_dirs <- c(
    'C:/my_develop_workshop',
    'C:/scripts',
    'C:/Users/Ashel/Documents',
    'C:/Users/persey/Documents'
  )
  
  # Состояние для хранения результатов
  search_data <- reactiveVal(NULL)
  
  observeEvent(input$search_btn, {
    req(input$file_pattern)
    pattern <- input$file_pattern
    
    results <- map_dfr(
      search_dirs,
      ~ {
        fif <- findInFiles(
          extensions = c('R', 'py'), 
          pattern    = pattern, 
          root       = .x,
          output     = "tibble"
        ) %>% 
          as_tibble() %>% 
          mutate(file = as.character(file.path(.x, file)))
        
        if (nrow(fif) == 0) {
          return(tibble())
        } else {
          return(fif)
        }
      }
    )
    
    search_data(results)
  })
  
  # UI чат-истории
  output$file_search_ui <- renderUI({
    pattern <- input$file_pattern
    if (!is.null(pattern) && nzchar(pattern)) {
      div(
        style = "margin-bottom: 10px; padding: 10px; background-color: #D6EAF8; border-radius: 10px;",
        strong("Вы искали:"), br(),
        tags$pre(style = "white-space: pre-wrap;", pattern)
      )
    }
  })
  
  # Вывод результатов
  output$search_results <- DT::renderDataTable({
    df <- search_data()
    req(df)
    validate(need(nrow(df) > 0, message = FALSE))
    
    df_chr <- df %>%
      mutate(
        match = format(match),              # заменить as.character на format
        across(-match, as.character)        # остальные колонки — в текст
      )
    
    DT::datatable(
      df_chr,
      options = list(pageLength = 10),
      rownames = FALSE,
      escape = FALSE
    )
  })
  
  # Сообщение, если ничего не найдено
  output$search_message <- renderUI({
    df <- search_data()
    if (!is.null(df) && nrow(df) == 0) {
      div(
        style = "margin-top: 10px; padding: 10px; background-color: #FADBD8; border-radius: 10px;",
        strong("Файлы не найдены.")
      )
    }
  })
  
}

shinyApp(ui, server, options = list(host = "0.0.0.0", port = 3838))
