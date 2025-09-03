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
  app_con <- dbConnect(RSQLite::SQLite(), "app.db")
  
  observe({
    if (logged_in() && user_role() == "admin") {
      mod_access_server("access", conn = app_con, auth, session_id = session$token)
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
          
          # Модуль AI разработки
          tabPanel(
            "AI Ассистент",
            bslib::page_fluid(
              div(class = "stats-description",
                  HTML(glue::glue(
                    "Этот AI ассистент поможет вам в разработке R кода для запроса данных из наших внутренних сервисов с использованием наших внутренних пакетов:<Br>",
                    "<ul>",
                     "<li><code>rpup</code> - Пакет для работы с базой ПУПа</li>
                      <li><code>n1</code> - Пакет для работы с N1</li>
                      <li><code>pfworker</code> - Пакет для работы с Планфикс</li>
                      <li><code>segments</code> - Пакет для определения сегментов по списаниям или проект-услугам</li>
                      <li><code>serviceaccounts</code> - Пакет для авторизации в Google сервисах через сервисные аккаунты</li>
                      <li><code>alspy</code> - Python пакет для работы с базой ПУПа</li>",
                    "</ul>",
                    "<Br>Так же он умеет работать с задачами на сервере аналитики, бот умеет:<Br>",
                    "<ul>",
                     "<li>Искать нужную задачу по вашему описанию</li>
                      <li>По названию задачу давать вам информацию о ней из планировщика заданий</li>
                      <li>По названию задачи читать скрипт который она запускает, и читать лог его последнего выполнения</li>
                      <li>Быстро проверять есть ли задачи работа которых была оставлена ошибкой</li>
                      <li>Запускать задачи на сервере</li>
                      <li>Активировать и деактивировать задачи в планировщике заданий",
                    "</ul>",
                    "<Br>Умеет запрашивать данные из Планфикса:<Br>",
                    "<ul>",
                     "<li>Выводить список задач по спринтам аналитиков по любому месяцу</li>
                      <li>Получать информацию по любой задаче из планфикса по ссылке</li>
                      <li>Выводить информацию по юнит экономике за любой месяц</li>",
                    "</ul>",
                    "<Br><Br>Более подробно ознакомится с возможностями данного AI Ассистента можно в <a href='https://youtu.be/sQRPMJYIxMA' target='_blank'>этом видео</a>."
                  )
                ),
              )
              ,
              chat_ui("simple_chat")
              ,
              # Добавляем кнопку сброса чата
              div(class = "chat-controls", style = "margin-top: 15px; text-align: center;",
                  actionButton(
                    "reset_chat", 
                    "Сбросить чат", 
                    icon = icon("refresh"), 
                    class = "btn-warning btn-sm"
                  )
              )
            )
          ),
          
          # Поиск по файлам — только для admin и user
          if (user_role() %in% c("admin", "user")) {
            mod_tab_find_in_files_ui("file_search")
          },
          
          # Процессы — только для admin и user
          if (user_role() %in% c("admin", "user")) {
            mod_tab_processes_ui("processes_tab")
          },
          
          # CMD только для admin и user
          if (user_role() %in% c("admin", "user")) {
            mod_tab_cmd_ui("cmd")
          },
          
          # Доступы — только для admin
          if (user_role() == "admin") {
            tabPanel("Доступ", mod_access_ui("access"))
          },
          
          # Улучшенная вкладка "Статистика"
          mod_tab_statistic_ui("stats_tab"),
          
          # Вкладка логов
          if (user_role() == "admin") {
            mod_tab_logs_ui("logs_tab")
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
        # CSS для popup который появляется при клике на таблицу задач
        tags$style(HTML("
          .modal-content {
            background-color: #2b2b2b;   /* тёмный фон */
            color: #f0f0f0;              /* светлый текст */
          }
          .modal-header, .modal-footer {
            border: none;                /* убираем белые бордеры */
          }
          .modal-title {
            color: #ffffff;              /* заголовок яркий */
            font-weight: bold;
          }
          .modal-body strong {
            color: #c9e6ff;              /* выделение для strong */
          }
        ")),
        
        tags$script(HTML("
          (function waitForChat() {
            var el = document.getElementById('simple_chat');
            if (el) {
              Shiny.setInputValue('simple_chat_ready', true, {priority: 'event'});
            } else {
              setTimeout(waitForChat, 150);
            }
          })();
        ")),
        
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
  all_tasks <- reactiveVal(NULL)
  services_store <- reactiveVal(NULL)
  processes_store <- reactiveVal(NULL)
  session_store <- reactiveVal(NULL)
  action_store  <- reactiveVal(NULL)
  
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
        
        # как только всё трое завершатся — прячем общий лоадер
        promise_all(
          tasks = p_tasks,
          services = p_services,
          processes = p_process,
          sessions = p_sessions,
          actions  = p_actions
        ) %...>% with({
          # Этот блок выполняется когда всё завершено
          all_tasks(tasks)
          services_store(services)
          processes_store(processes)
          session_store(sessions)
          action_store(actions)
          waiter_hide()
          showNotification("Все данные загружены", type = "message", duration = 3)
        }) %...!% (function(e) {
          waiter_hide()
          showNotification(paste("Ошибка общей загрузки:", conditionMessage(e)), type = "error", duration = 6)
        })
        
        #waiter_hide()
      })
      
      # модуль служб ------------------------------------------------------------
      mod_tab_services_server("services_tab", services_data, user_role, auth, session_id = session$token)
      
      # Модуль вкладки задач ----------------------------------------------------
      mod_tab_tasks_server("tasks_tab", all_tasks, user_role, auth, session_id = session$token)
      
      # Модуль статистики
      mod_tab_statistic_server("stats_tab", all_tasks)
      
      # Модуль AI чата - добавлен напрямую в код (вне модулей)
      # В серверной части - создаем реактивное значение для чата
      dev_chat <- reactiveVal()
      
      # Инициализируем чат при первом запуске
      observe({
        if (logged_in() && is.null(dev_chat())) {
          new_chat <- create_new_chat(user_role())
          dev_chat(new_chat)
        }
      })
      
      # Приветственное сообщение в чате (как было изначально)
      observeEvent(input$simple_chat_ready, {
        req(input$simple_chat_ready)
        usr <- auth$user()
        if (!is.null(usr) && !is.null(usr$login) && nzchar(usr$login)) {
          chat_append(
            "simple_chat", 
            paste0("👋 Привет, <b>", snakecase::to_title_case(usr$login), 
                   "</b>!<Br><Br>Я умею писать код для работы со всеми внутренними источниками данных, такими как ПУП, N1, Планфикс, умею работать со скриптами на сервере аналитики, а так же запрашивать информацию о задачах из Планфикс.<Br><Br>Чем могу тебе помочь?")
          )
          return()
        }
      }, once = TRUE)
      
      # Рабочий обработчик кнопки сброса чата
      observeEvent(input$reset_chat, {
        
        write_action_log(user = auth$user()$login, func = 'AI Assistant Reset Chat', session_id = session$token)
        
        # Пересоздаем объект чата
        new_chat <- create_new_chat()
        dev_chat(new_chat)
        
        # Добавляем сообщение о сбросе в чат
        chat_append(
          "simple_chat", 
          "🔄 <b>Контекст чата сброшен.</b> Я забыл всю предыдущую историю и готов к новому диалогу!"
        )
        
        # Показываем уведомление
        showNotification("Контекст чата сброшен. Бот забыл всю предыдущую историю.", type = "message", duration = 5)
      })
      
      # Обработчик пользовательского ввода
      observeEvent(input$simple_chat_user_input, {
        req(dev_chat()) # Проверяем что объект чата существует
        write_action_log(user = auth$user()$login, func = 'AI Assistant', session_id = session$token, value = input$simple_chat_user_input)
        message("Получен ввод:", input$simple_chat_user_input)
        stream <- dev_chat()$stream_async(input$simple_chat_user_input)
        chat_append("simple_chat", stream)
      })
      
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
      mod_tab_logs_server("logs_tab", session_store, action_store, logs_last_update)
      
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
    
    p_tasks    %...>% (function(x) all_tasks(x))
    p_services %...>% (function(x) services_store(x))
    p_process  %...>% (function(x) processes_store(x))
    p_sessions %...>% (function(x) session_store(x))
    p_actions  %...>% (function(x) action_store(x))
    
    promise_all(
      tasks    = p_tasks,
      services = p_services,
      process  = p_process,
      sessions = p_sessions,
      actions  = p_actions
    ) %...>% with({
      all_tasks(tasks)
      services_store(services)
      processes_store(process)
      session_store(sessions)
      action_store(actions)
      
      logs_last_update(lubridate::with_tz(Sys.time(), "Europe/Kyiv"))
      
      waiter_hide()
      showNotification("Все данные загружены", type = "message", duration = 3)
    }) %...!% (function(e) {
      waiter_hide()
      showNotification(paste("Ошибка общей загрузки:", conditionMessage(e)), type = "error", duration = 6)
    })
  })
  
}

if (system("git rev-parse --abbrev-ref HEAD", intern = TRUE) == 'master') {
  shinyApp(ui, server, options = list(host = "0.0.0.0", port = 3838))
} else {
  shinyApp(ui, server, options = list(port = 81))
}
