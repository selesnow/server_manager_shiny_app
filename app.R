# options
options(
  httr_config = httr::config(timeout = 9000, connecttimeout = 9000),
  ellmer_timeout_s = 600
)

# library
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
library(later)
library(ellmer)
library(bslib)
library(promises)
library(future)
library(forcats)
library(ggthemr)

# проверка создан ли конфиг
if (!file.exists('config.yaml')) source('create_config.R')

# Чтение конфига
conf <<- yaml::read_yaml('config.yaml')

# проверка развёрнута ли база данных
if (!file.exists(conf$database_settings$app_data_base)) source('create_db.R')

plan(multisession)
ggthemr('flat dark')

# Загрузка вспомогательных функций
for(fun in dir(here::here("R"))) if (fun == "desktop.ini") next else source(here::here("R", fun))

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
  
  options(shiny.error = function(...) {
    
    # Получаем последнее сообщение об ошибке
    last_error_msg <- geterrmessage()
    
    # Получаем стек вызовов
    call_stack <- sys.calls()
    
    # Формируем сообщение
    if (length(list(...)) > 0 && !is.null(list(...)[[1]])) {
      # Если объект ошибки передан
      e <- list(...)[[1]]
      msg <- if(is.null(e$call)) {
        as.character(e$message)
      } else {
        str_glue("Ошибка вызвана [{deparse(e$call, nlines = 1L)}]: {e$message}")
      }
    } else {
      # Если объект ошибки не передан, используем geterrmessage()
      msg <- str_glue("{last_error_msg}")
    }
    
    print(paste("Сообщение об ошибке:", msg))
    print(paste("Стек вызовов:", length(call_stack), "уровней"))
    
    tryCatch({
      error_log(
        session_id = session$token %||% "unknown", 
        user = session$userData$login %||% "unknown", 
        error = msg
      )
    }, error = function(err) {
      cat("Не удалось записать ошибку в базу:", msg, "\n")
    })
  })
  
  # Проверка авторизации ----------------------------------------------------
  # Подключение к базе данных SQLite
  # Коннект к БД
  app_con <- dbConnect(RSQLite::SQLite(), conf$database_settings$app_data_base)
  
  observe({
    if (logged_in() && user_role() %in% c('admin')) {
      mod_access_server("access", conn = app_con, auth = auth, session_id = session$token, conf_rv = conf_rv)
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
  #mod_auth_server("auth", logged_in, user_role, check_user_fun = check_user)
  auth <- mod_auth_server("auth", logged_in, user_role, check_user)
  
  # UI для основного контента
  output$app_ui <- renderUI({
    if (logged_in()) {
      
      # фиксируем старт сессии
      user_login <- auth$user()$login
      session$userData$login <- user_login
      session$userData$logged_in <- TRUE
      
      # фиксируем старт сессии
      session_log(
        session_id = session$token,
        user = auth$user()$login,
        action = 'start'
      )
      
      # фиксируем завершение сессии
      session$onSessionEnded(function() {
        # Этот код ГАРАНТИРОВАННО выполнится при:
        # ✅ Закрытии браузера пользователем
        # ✅ Потере соединения WebSocket 
        # ✅ Зависании R-процесса (если сервер еще жив)
        # ✅ Таймауте соединения
        # ✅ Перезагрузке страницы
        # ✅ "Замутнении" интерфейса
        
        if (isolate(logged_in())) {
          session_log(
            session_id = session$token,
            user = session$userData$login,
            action = 'end'
          )
        }
        
        message("Session ended: ", session$token)
      })
      
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
          title = tagList(
            tags$img(src = "favicon.png", height = "30px", style = "vertical-align: middle; margin-right: 10px;"),
            str_glue("{yaml::read_yaml(here::here('app_info.yml'))$name} v{yaml::read_yaml(here::here('app_info.yml'))$version}")
          ),
          windowTitle = "Server Manager"
        ),
        
        # Добавляем элементы управления отдельно, после заголовка
        div(
          class = "header-container",
          style = "display: flex; flex-direction: column; align-items: flex-start;",
          
          # Инфо о пользователе
          div(
            class = "text-primary",
            style = "font-size: 0.9em; margin-bottom: 5px;",
            glue::glue("Пользователь: {user_login} ({user_role()})")
          ),
          
          # общие кнопки управления
          div(
            class = "controls-wrapper", 
            div(
              class = "controls-container", 
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
          if (user_role() %in% conf$access_managemet$Задачи) {
            mod_tab_tasks_ui("tasks_tab")
          },
          
          # Вкладка "Службы"
          if (user_role() %in% conf$access_managemet$Службы) {
            mod_tab_services_ui("services_tab")
          },
          
          # Модуль AI разработки
          if (user_role() %in% conf$access_managemet$`AI Ассистент`) {
            mod_tab_ai_assistant_ui("ai_tab")
          },
          
          # Поиск по файлам — только для admin и user
          if (user_role() %in% conf$access_managemet$`Поиск по файлам`) {
            mod_tab_find_in_files_ui("file_search")
          },
          
          # Процессы — только для admin и user
          if (user_role() %in% conf$access_managemet$Процессы) {
            mod_tab_processes_ui("processes_tab")
          },
          
          # CMD только для admin и user
          if (user_role() %in% conf$access_managemet$CMD) {
            mod_tab_cmd_ui("cmd")
          },
          
          # Доступы — только для admin
          if (user_role() %in% conf$access_managemet$Доступ) {
            tabPanel("Доступ", mod_access_ui("access"))
          },
          
          # Улучшенная вкладка "Статистика"
          if (user_role() %in% conf$access_managemet$Статистика) {
            mod_tab_statistic_ui("stats_tab")
          },
          
          # Вкладка логов
          if (user_role() %in% conf$access_managemet$Логи) {
            mod_tab_logs_ui("logs_tab")
          },
          # Помощь и обновления
          if (user_role() %in% conf$access_managemet$Readme) {
            mod_help_ui('help')
          },
          if (user_role() %in% conf$access_managemet$News) {
            mod_news_ui('news')
          }
        ),
        
        # скрипт копирования названия файла в буфер обмена на вкладке поиска по файлам
        tags$script("
          Shiny.addCustomMessageHandler('copyToClipboard', function(message) {
            navigator.clipboard.writeText(message).then(function() {
              console.log('Copied to clipboard: ' + message);
            }, function(err) {
              console.error('Failed to copy: ', err);
            });
          });
        "),
        
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
        ")         
        )
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
  
  # Инфо по задачам, процессам и службам
  all_tasks           <- reactiveVal(NULL)
  services_store      <- reactiveVal(NULL)
  processes_store     <- reactiveVal(NULL)
  session_store       <- reactiveVal(NULL)
  action_store        <- reactiveVal(NULL)
  task_triggers_store <- reactiveVal(NULL)
  conf_rv             <- reactiveVal(conf)
  
  # фиксация изменения логов
  logs_last_update <- reactiveVal(lubridate::with_tz(Sys.time(), "Europe/Kyiv"))
  
  # Добавляем реактивное значение для отслеживания обновлений
  refresh_trigger <- reactiveVal(0)
  
  # Модифицируем реактивное значение services_data, чтобы оно зависело от refresh_trigger
  services_data <- reactive({
    services_store()
  })
  
  # Основная логика приложения, запускается после логина
  observeEvent(logged_in(), {
    if (logged_in()) {
      
      observe({

        # показываем общий лоадер интерфейса
        waiter_show(
          html = HTML(paste(spin_fading_circles(), br(), h4("Загрузка интерфейса..."))),
          color = "#333"
        )
        
        # параллельные запросы
        p_tasks    <- future_promise({ get_tasks() })
        p_services <- future_promise({ get_services() })
        p_process  <- future_promise({ get_processes() })
        p_sessions <- future_promise({ get_session_log() })
        p_actions  <- future_promise({ get_action_log() })
        p_triggers <- future_promise({ get_task_triggers() })
        
        # обновляем хранилища по мере готовности (UI не блокируется)
        p_tasks %...>% (function(x) {
          all_tasks(x)
          showNotification("Задачи загружены", type = "message", duration = 2)
        }) %...!% (function(e) {
          showNotification(paste("Ошибка загрузки задач:", conditionMessage(e)), type = "error", duration = 6)
        })
        
        p_services %...>% (function(x) {
          services_store(x)
          showNotification("Службы загружены", type = "message", duration = 2)
        }) %...!% (function(e) {
          showNotification(paste("Ошибка загрузки служб:", conditionMessage(e)), type = "error", duration = 6)
        })
        
        p_process %...>% (function(x) {
          processes_store(x)
          showNotification("Процессы загружены", type = "message", duration = 2)
        }) %...!% (function(e) {
          showNotification(paste("Ошибка загрузки процессов:", conditionMessage(e)), type = "error", duration = 6)
        })
        
        p_sessions %...>% (function(x) {
          session_store(x)
          showNotification("Логи сессий загружены", type = "message", duration = 2)
        })
        
        p_actions  %...>% (function(x) {
          action_store(x)
          showNotification("Логи действий загружены", type = "message", duration = 2)
        })
        
        p_triggers %...>% (function(x) {
          task_triggers_store(x)
          showNotification("Триггеры задач загружены", type = "message", duration = 2)
        }) %...!% (function(e) {
          showNotification(paste("Ошибка загрузки триггеров задач:", conditionMessage(e)), type = "error", duration = 6)
        })
        
        # как только всё трое завершатся — прячем общий лоадер
        promise_all(
          tasks     = p_tasks,
          services  = p_services,
          processes = p_process,
          sessions  = p_sessions,
          actions   = p_actions,
          triggers  = p_triggers
        ) %...>% with({
          # Этот блок выполняется когда всё завершено
          all_tasks(tasks)
          services_store(services)
          processes_store(processes)
          session_store(sessions)
          action_store(actions)
          task_triggers_store(triggers) 
          waiter_hide()
          showNotification("Все данные загружены", type = "message", duration = 3)
        }) %...!% (function(e) {
          waiter_hide()
          showNotification(paste("Ошибка общей загрузки:", conditionMessage(e)), type = "error", duration = 6)
        })
        
        #waiter_hide()
      })
      
      # модуль служб ------------------------------------------------------------
      mod_tab_services_server("services_tab", services_data, user_role, auth, session_id = session$token, conf_rv)
      
      # Модуль вкладки задач ----------------------------------------------------
      mod_tab_tasks_server("tasks_tab", all_tasks, task_triggers_store, user_role, auth, session_id = session$token, conf_rv)
      
      # Модуль статистики
      mod_tab_statistic_server("stats_tab", all_tasks, conf_rv)
      
      # Модуль AI ассистент
      mod_tab_ai_assistant_server(
        "ai_tab",
        auth = auth,
        user_role = user_role,
        conf_rv = conf_rv,
        session_id = session$token
      )
      
      # Модуль процессов
      process_data <- reactive({
        processes_store()
      })
      
      # модуль процессов
      mod_tab_processes_server("processes_tab", process_data = process_data, auth, session_id = session$token)
      
      # Модуль поощь и новости
      mod_help_server("help")
      mod_news_server("news")
      
      # Модуль логов
      mod_tab_logs_server("logs_tab", session_store, action_store, logs_last_update, conf_rv, auth, session_id = session$token)
      
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
  mod_tab_cmd_server("cmd", auth, session_id = session$token)
  
  # Поиск по файлам ---------------------------------------------------------
  mod_tab_find_in_files_server("file_search", all_tasks, auth, session_id = session$token)
  
  # Модифицируем обработчик для кнопки обновления данных
  observeEvent(input$refresh_data, {
    waiter_show(
      html = HTML(paste(spin_fading_circles(), br(), h4("Обновляем данные..."))),
      color = "#333"
    )
    
    p_tasks    <- future_promise({ get_tasks() })
    p_services <- future_promise({ get_services() })
    p_process  <- future_promise({ get_processes() })
    p_sessions <- future_promise({ get_session_log() })
    p_actions  <- future_promise({ get_action_log() })
    p_triggers <- future_promise({ get_task_triggers() })
    
    p_tasks    %...>% (function(x) all_tasks(x))
    p_services %...>% (function(x) services_store(x))
    p_process  %...>% (function(x) processes_store(x))
    p_sessions %...>% (function(x) session_store(x))
    p_actions  %...>% (function(x) action_store(x))
    p_triggers %...>% (function(x) task_triggers_store(x))
    
    promise_all(
      tasks    = p_tasks,
      services = p_services,
      process  = p_process,
      sessions = p_sessions,
      actions  = p_actions,
      triggers = p_triggers
    ) %...>% with({
      all_tasks(tasks)
      services_store(services)
      processes_store(process)
      session_store(sessions)
      action_store(actions)
      task_triggers_store(triggers)
      
      logs_last_update(lubridate::with_tz(Sys.time(), "Europe/Kyiv"))
      
      waiter_hide()
      showNotification("Все данные загружены", type = "message", duration = 3)
    }) %...!% (function(e) {
      waiter_hide()
      showNotification(paste("Ошибка общей загрузки:", conditionMessage(e)), type = "error", duration = 6)
    })
  })
  
}

# if (!interactive()) {
#   runApp('C:/scripts/alsey/netpeak_core/nc_analytics_team/shiny_task_service_manager', port = 8080, host = '0.0.0.0')
# }

if (system("git rev-parse --abbrev-ref HEAD", intern = TRUE) == 'master') {
  shinyApp(ui, server, options = list(host = "0.0.0.0", port = 3838))
} else {
  shinyApp(ui, server, options = list(port = 81))
}
