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
library(ps)
library(tidyr)
library(waiter)

# Загрузка вспомогательных функций
for(fun in dir(here::here("R"))) source(here::here("R", fun))

# Загрузка модулей
for(mod in dir(here::here("modules"))) source(here::here("modules", mod))

# Генерация интерфейса ----------------------------------------------------
ui <- fluidPage(
  useWaiter(),             # Помощник в загрузке приложения
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
          title = str_glue("Server Task & Service Manager v{yaml::read_yaml(here::here('app_info.yml'))$version}"),
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
        # Начало вкладок
        tabsetPanel(
          id = "main_tabs",
          
          # Вкладка "Задачи"
          mod_tab_tasks_ui("tasks_tab"),
          
          # Вкладка "Службы"
          mod_tab_services_ui("services_tab"),
          
          # Улучшенная вкладка "Статистика"
          mod_tab_statistic_ui("stats_tab"),
          
          # CMD только для admin и user
          if (user_role() %in% c("admin", "user")) {
            mod_tab_cmd_ui("cmd")
          },
          
          # Процессы — только для admin и user
          if (user_role() %in% c("admin", "user")) {
            mod_tab_processes_ui("processes_tab")
          },
          
          # Поиск по файлам — только для admin и user
          if (user_role() %in% c("admin", "user")) {
            mod_tab_find_in_files_ui("file_search")
          },
          
          # Доступы — только для admin
          if (user_role() == "admin") {
            tabPanel("Доступ", mod_access_ui("access"))
          },
          
          # Помощь и обновления
          mod_help_ui('help'),
          mod_news_ui('news')
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
    waiter_show(
      html = HTML(paste(
        spin_fading_circles(),
        br(),
        h4("Загрузка служб...")
      )),
      color = "#333"
    )
    # Это заставит services_data пересчитываться каждый раз при изменении refresh_trigger
    refresh_trigger()
    services <- get_services()
    #waiter_hide()
    return(services)
    
  })
  
  # Основная логика приложения, запускается после логина
  observeEvent(logged_in(), {
    if (logged_in()) {
      
      waiter_show(
        html = HTML(paste(
          spin_fading_circles(),
          br(),
          h4("Загрузка интерфейса...")
        )),
        color = "#333"
      )
      
      observe({
        waiter_show(
          html = HTML(paste(
            spin_fading_circles(),
            br(),
            h4("Загрузка данных планировщика заданий...")
          )),
          color = "#333"
        )
        all_tasks(get_tasks())
        #waiter_hide()
      })
      
      # модуль служб ------------------------------------------------------------
      mod_tab_services_server("services_tab", services_data, user_role)
      
      # Модуль вкладки задач ----------------------------------------------------
      mod_tab_tasks_server("tasks_tab", all_tasks, user_role)
      
      # Модуль статистики
      mod_tab_statistic_server("stats_tab", all_tasks)
      
      # Модуль процессов
      process_data <- reactive({
        get_processes()
      })
      
      #mod_tab_processes_server("processes_tab", process_data)
      mod_tab_processes_server("processes_tab", refresh_trigger = refresh_trigger)
      
      # Модуль поощь и новости
      mod_help_server("help")
      mod_news_server("news")
      
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
      waiter_hide()
    }
  }
  )
  
  # Командная строка --------------------------------------------------------
  mod_tab_cmd_server("cmd")
  
  # Поиск по файлам ---------------------------------------------------------
  mod_tab_find_in_files_server("file_search")

  # Модифицируем обработчик для кнопки обновления данных
  observeEvent(input$refresh_data, {
    
    
    waiter_show(
        html = HTML(paste(
          spin_fading_circles(),
          br(),
          h4("Загрузка данных планировщика заданий...")
        )),
        color = "#333"
    )
    # Обновляем данные о задачах
    all_tasks(get_tasks())
    
    #waiter_hide()
    
    # Увеличиваем значение refresh_trigger, что вызовет перерасчет services_data()
    refresh_trigger(refresh_trigger() + 1)
    
    # Показываем уведомление об успешном обновлении
    showNotification("Данные успешно обновлены", type = "message", duration = 3)
    
  })
  
}

if (system("git rev-parse --abbrev-ref HEAD", intern = TRUE) == 'master') {
  shinyApp(ui, server, options = list(host = "0.0.0.0", port = 3838))
} else {
  shinyApp(ui, server, options = list(port = 8080))
}
