# options
options(
  httr_config = httr::config(timeout = 9000, connecttimeout = 9000),
  ellmer_timeout_s = 600
)

# Rout
# --- путь к приложению Shiny ---
app_dir <- "C:/scripts/alsey/netpeak_core/nc_analytics_team/shiny_task_service_manager"

# --- путь к лог-файлу в стиле R CMD BATCH ---
log_file <- file.path(app_dir, "app.Rout")

# перенаправляем stdout и stderr в app.Rout
if (!interactive()) {
  
  # создаём соединение для stdout
  out_con <- file(log_file, open = "a")  # append = TRUE
  sink(out_con, type = "output")         # для обычного вывода
  
  # создаём отдельное соединение для сообщений / ошибок
  msg_con <- file(log_file, open = "a")
  sink(msg_con, type = "message")
  
}

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
library(appauth)   # авторизация: вход, пароли, блокировки, сессии, права
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
library(lubridate)

# config.yaml у dev и prod свой и в git не хранится: создаём из значений по умолчанию
# или дописываем в существующий недостающие настройки, значения не перезаписываются
source('create_config.R')
ensure_config('config.yaml')

# Чтение конфига
conf <<- yaml::read_yaml('config.yaml')

plan(multisession)
ggthemr('flat dark')

# Загрузка вспомогательных функций
for(fun in dir(here::here("R"))) if (fun == "desktop.ini") next else source(here::here("R", fun))

# проверка развёрнута ли база данных: если файла нет (например, в настройках указана новая база)
# или это пустая база без таблиц, база создаётся с нуля, старая база не затрагивается
if (database_needs_creation(conf$database_settings$app_data_base)) source('create_db.R')

# настройки авторизации: раздел security из config.yaml, значения по умолчанию из пакета
# (функция build_auth_settings из R/auth_events.R). Присваивание глобальное (<<-), как у conf:
# функции из папки R/ (log_auth_event, admin_only_provider, require_permission) ищут
# auth_settings в глобальном окружении, а app.R при запуске через runApp() загружается
# в собственное окружение
auth_settings <<- build_auth_settings(conf$security)

# Синхронизация справочника действий для логов (добавляет недостающие, существующие не меняет)
local({
  con <- dbConnect(RSQLite::SQLite(), conf$database_settings$app_data_base)
  on.exit(dbDisconnect(con))
  # авторизация (пакет appauth): достраивает схему в уже работающей базе (dev и prod
  # базы разные, поэтому схема обновляется при запуске, а не отдельным скриптом):
  # таблицы попыток входа, токенов, колонки blocked и must_change; чистит устаревшие данные
  aa_prepare_db(con, auth_settings)
  n_added <- sync_actions(con)
  if (n_added > 0) message("Добавлено действий в справочник actions: ", n_added)
})

# Загрузка модулей
for(mod in dir(here::here("modules"))) if (mod == "desktop.ini") next else source(here::here("modules", mod))

# Генерация интерфейса ----------------------------------------------------
ui <- fluidPage(
  tags$head(
    # Подключаем шрифты Google Fonts напрямую в HTML для надежности
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@300;400;500;600;700;800&display=swap"),
    
    # Добавляем иконку для вкладки браузера
    tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
    
    # Подключаем внешние CSS файлы с cache-busting для сброса кэша браузера
    tags$link(rel = "stylesheet", type = "text/css", href = "css/header-styles.css?v=10"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/main-styles.css?v=13"),
    # стиль календаря
    tags$link(rel = "stylesheet", type = "text/css", href = "css/calendar.css?v=9")
  ),
  aa_app_head(),           # стили и скрипты авторизации (cookie автовхода)
  useWaiter(),             # Помощник в загрузке приложения
  aa_auth_ui("auth"),      # Модуль авторизации (пакет appauth)
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
  
  # Авторизация (пакет appauth) ---------------------------------------------
  # подключение закрывается вместе с сессией: иначе оно остаётся открытым до
  # остановки процесса R и держит файл базы (его нельзя переименовать или удалить)
  app_con <- aa_session_connection(conf$database_settings$app_data_base)

  # вход по паролю и по cookie автовхода, защита от подбора пароля, выход.
  # Имя cookie прежнее (auth_token): токены, выданные до перехода на пакет, продолжают работать
  auth <- aa_auth_server(
    "auth", app_con, auth_settings,
    on_event    = log_auth_event,
    title       = "Server Manager",
    logo        = "favicon.png",
    cookie_name = "auth_token"
  )

  # прежние имена: на них опираются остальные модули приложения
  logged_in <- auth$logged_in
  user_role <- auth$role

  # блокировка, удаление, смена роли и сброс пароля вступают в силу в течение
  # session_check_seconds (30 секунд): открытая сессия закрывается при проверке
  aa_session_guard(auth, app_con, auth_settings)

  # управление доступом только для администратора: серверная часть вкладки
  # регистрируется только ему (проверка роли дублируется внутри модуля)
  aa_register_module(auth, "Доступ", admin_only_provider, function() {
    mod_access_server("access", conn = app_con, auth = auth, session_id = session$token, conf_rv = conf_rv)
  })

  # UI для основного контента
  output$app_ui <- renderUI({
    if (logged_in()) {
      
      # фиксируем старт сессии
      usr <- auth$user()
      req(usr, usr$login)
      
      user_login <- usr$login
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
        
        # Стили загружаются глобально в статическом UI, предотвращая конфликты при перерисовке renderUI
        
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
            icon("user"),
            glue::glue("Пользователь: {user_login} ({user_role()})")
          ),
          
          # общие кнопки управления
          div(
            class = "controls-wrapper", 
            div(
              class = "controls-container", 
              actionButton("refresh_data", "Обновить данные", icon = icon("refresh"), class = "btn-warning"),
              actionButton("change_password_btn", "Сменить пароль", icon = icon("key"), class = "btn-secondary btn-sm ml-2"),
              actionButton("logout_all_btn", "Выйти на всех устройствах", icon = icon("right-from-bracket"), class = "btn-secondary btn-sm ml-2"),
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
            usr <- auth$user()
            greeting <- if (!is.null(usr) && nzchar(usr$login)) {
              paste0(
                "👋 Привет, <b>", snakecase::to_title_case(usr$login),
                "</b>!<br><br>Я умею писать код для работы со всеми внутренними источниками данных, такими как ПУП, N1, Планфикс, умею работать со скриптами на сервере аналитики, а так же запрашивать информацию о задачах из Планфикс.<br><br>Чем могу тебе помочь?"
              )
            } else {
              "👋 Привет!<br><br>Я умею писать код для работы со всеми внутренними источниками данных...<br><br>Чем могу помочь?"
            }
            
            mod_tab_ai_assistant_ui("ai_tab", messages = greeting)
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
        
        # JavaScript для переключения темной/светлой темы и горячих клавиш
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

            // Горячие клавиши Ctrl + Shift + U для обновления данных
            $(document).on('keydown', function(e) {
              if (e.ctrlKey && e.shiftKey && (e.key === 'U' || e.key === 'u' || e.code === 'KeyU')) {
                e.preventDefault();
                $('#refresh_data').click();
              }
            });
          });
        ")         
        )
      )
    }
  })
  
  # Выход по кнопке "Выйти": отзывает токен, чистит cookie, перезагружает страницу,
  # чтобы зарегистрированные в сессии модули прежнего пользователя исчезли
  observeEvent(input$logout_btn, auth$logout(reload = TRUE))

  # Смена своего пароля: форма модуля appauth в окне, после успешной смены окно закрывается.
  # Токены остальных сессий пользователя отзываются, текущая сессия продолжает работать.
  pw <- aa_password_server("password", app_con, auth, auth_settings, on_event = log_auth_event)
  observeEvent(input$change_password_btn, {
    showModal(modalDialog(
      aa_password_ui("password"),
      title = "Смена пароля", easyClose = TRUE, footer = modalButton("Закрыть")
    ))
  })
  observeEvent(pw$changed(), removeModal(), ignoreInit = TRUE)

  # «Выйти на всех устройствах» только после подтверждения: кнопка завершает и текущую
  # сессию, и сессии на других устройствах (их закроет проверка сессий раз в 30 секунд)
  observeEvent(input$logout_all_btn, {
    showModal(modalDialog(
      "Все ваши сессии на всех устройствах будут завершены, в приложение придётся войти заново.",
      title = "Выйти на всех устройствах?", easyClose = TRUE,
      footer = tagList(
        modalButton("Отмена"),
        actionButton("logout_all_confirm", "Выйти везде", class = "btn-danger")
      )
    ))
  })
  observeEvent(input$logout_all_confirm, {
    removeModal()
    auth$logout_all(reload = TRUE)
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
      
      # Серверные модули регистрируются только если у роли пользователя есть
      # право на вкладку, иначе подделанные запросы не найдут их обработчиков.
      # Права действий внутри модулей дополнительно проверяет require_permission().
      can <- function(permission) user_can(auth$user()$role, permission, conf_rv())

      # модуль служб ------------------------------------------------------------
      if (can("Службы")) {
        mod_tab_services_server("services_tab", services_data, user_role, auth, session_id = session$token, conf_rv)
      }

      # Модуль вкладки задач ----------------------------------------------------
      if (can("Задачи")) {
        mod_tab_tasks_server("tasks_tab", all_tasks, task_triggers_store, user_role, auth, session_id = session$token, conf_rv)
      }

      # Модуль статистики
      if (can("Статистика")) {
        mod_tab_statistic_server("stats_tab", all_tasks, task_triggers_store, conf_rv)
      }

      # Модуль AI ассистент
      if (can("AI Ассистент")) {
        mod_tab_ai_assistant_server(
          "ai_tab",
          auth = auth,
          user_role = user_role,
          conf_rv = conf_rv,
          session_id = session$token,
          active = reactive({ input$main_tabs == "AI Ассистент" })
        )
      }

      # Модуль процессов
      process_data <- reactive({
        processes_store()
      })

      if (can("Процессы")) {
        mod_tab_processes_server("processes_tab", process_data = process_data, auth, session_id = session$token, conf_rv = conf_rv)
      }

      # Модуль поощь и новости
      if (can("Readme")) mod_help_server("help")
      if (can("News"))   mod_news_server("news")

      # Модуль логов
      if (can("Логи")) {
        mod_tab_logs_server("logs_tab", session_store, action_store, logs_last_update, conf_rv, auth, session_id = session$token)
      }

      # Командная строка
      if (can("CMD")) {
        mod_tab_cmd_server("cmd", auth, session_id = session$token, conf_rv = conf_rv)
      }

      # Поиск по файлам
      if (can("Поиск по файлам")) {
        mod_tab_find_in_files_server("file_search", all_tasks, auth, session_id = session$token, conf_rv = conf_rv)
      }

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
  
  # Модифицируем обработчик для кнопки обновления данных
  observeEvent(input$refresh_data, {
    req(logged_in())
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

if (system("git rev-parse --abbrev-ref HEAD", intern = TRUE) == 'master') {
  shinyApp(ui, server, options = list(host = "0.0.0.0", port = 81))
} else {
  shinyApp(ui, server, options = list(port = 81))
}
