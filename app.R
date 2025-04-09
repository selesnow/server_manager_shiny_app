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
# Загрузка модуля поиска по файлам
source("modules/mod_tab_find_in_files.R")
# Загрузка модуля поиска по файлам
source("modules/mod_tab_tasks.R")

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
            # Подключаем внешние CSS файлы
            tags$link(rel = "stylesheet", type = "text/css", href = "css/header-styles.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "css/main-styles.css")
          )
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
                    actionButton("refresh_data", "Обновить данные", icon = icon("refresh"), class = "btn-warning"),
                    actionButton("logout_btn", "Выйти",  icon = icon("power-off"), class = "btn-danger btn-sm ml-2"),
                    actionButton("toggle_theme", label = "Светлая тема", icon = icon("moon"), class = "btn-secondary btn-sm")
                )
            )
        ),
        
        # Начало вкладок
        tabsetPanel(
          id = "main_tabs",
          
          # Вкладка "Задачи"
          mod_tab_tasks_ui("tasks_tab"),
          
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
          mod_tab_find_in_files_ui("file_search"),
          
          # Доступы
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
        
        # JavaScript для переключения темной/светлой темы
        tags$script(HTML("
          $(document).ready(function() {
            // Темная тема по умолчанию
            $('#toggle_theme').on('click', function() {
              if ($('body').hasClass('light-mode')) {
                $('body').removeClass('light-mode');
                $('#toggle_theme').html('<i class=\"fa fa-sun\"></i> Светлая тема');  // Текст на кнопке
                $('#toggle_theme').removeClass('btn-light').addClass('btn-dark');  // Класс кнопки
              } else {
                $('body').addClass('light-mode');
                $('#toggle_theme').html('<i class=\"fa fa-moon\"></i> Тёмная тема');  // Текст на кнопке
                $('#toggle_theme').removeClass('btn-dark').addClass('btn-light');  // Класс кнопки
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
      
      # модуль служб ------------------------------------------------------------
      mod_tab_services_server("services_tab", services_data)
      
      
      # Модуль вкладки задач ----------------------------------------------------
      mod_tab_tasks_server("tasks_tab", all_tasks)
      
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
  
  # Вывод дополнительный информации о выбранной задаче ----------------------
  selected_task_details <- reactive({
    req(input$selected_task)
    all_tasks() %>% 
      filter(TaskName == input$selected_task) %>%
      select(TaskName, Author, `Run As User`, `Start In`, `Task To Run`, Client, Comment, `Last Run Time`, `Last Result`)
  })
  
  # Показываем информацию при выборе задачи
  observeEvent(input$selected_task, {
    shinyjs::show(id = "task_info_card")
  })
  
  # Командная строка --------------------------------------------------------
  mod_tab_cmd_server("cmd")
  
  # Поиск по файлам ---------------------------------------------------------
  mod_tab_find_in_files_server("file_search")
  
}

shinyApp(ui, server, options = list(host = "0.0.0.0", port = 3838))
