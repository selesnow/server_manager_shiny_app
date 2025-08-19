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
  #mod_auth_server("auth", logged_in, user_role, check_user_fun = check_user)
  auth <- mod_auth_server("auth", logged_in, user_role, check_user)
  
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
          
          # Улучшенная вкладка "Статистика"
          mod_tab_statistic_ui("stats_tab"),
          
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
              ),
              chat_ui("simple_chat")
            )
          ),
          
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
      
      # Модуль AI чата - добавлен напрямую в код (вне модулей)
      # Создаем чат с системным промптом
      dev_chat <- ellmer::chat_google_gemini(
        system_prompt = paste(readLines(here::here('ai_docs', 'system_prompt.md')), collapse = "\n"),
        model = 'gemini-2.0-flash',  
        echo  = 'none'
      )
      
      # добавление ассистенту инструментов
      dev_chat$register_tool(tool(
        Sys.Date,
        name = "get_current_date",
        description = "Получить сегодняшнюю дату, поможет при запросе данных за текущий спринт, или запросе расчётов по юнит экономике за текущий месяц"
      ))
      
      # Получить текст лога выполнения скрипта по названию задачи
      dev_chat$register_tool(tool(
        get_task_log,
        name = "get_task_log",
        description = "Получить текст лога выполнения скрипта по названию задачи из планировщика заданий Windows",
        arguments = list(
          task_name = type_string(
            "Название задачи из планировщика заданий Windows по которой надо получить лог выполнения (Rout файл) запускаемого скрипта"
          )
        )
      ))
      
      # Получить листинг скрипта запускаемого задачей
      dev_chat$register_tool(tool(
        get_task_script,
        name = "get_task_script",
        description = "Получить листинг скрипта запускаемого определённой задачей из планировщика заданий Windows по названию задачи",
        arguments = list(
          task_name = type_string(
            "Название задачи из планировщика заданий Windows по которой надо получить лог выполнения (Rout файл) запускаемого скрипта"
          )
        )
      ))
      
      # Получить информацию о задаче из планировщика
      dev_chat$register_tool(tool(
        get_task_info,
        name = "get_task_info",
        description = "Получить информацию об определённой задачей из планировщика заданий Windows по её названию.",
        arguments = list(
          task_name = type_string(
            "Название задачи из планировщика заданий Windows по которой надо получить лог выполнения (Rout файл) запускаемого скрипта"
          )
        )
      ))
      
      # Получить список упавших задач
      dev_chat$register_tool(tool(
        get_failed_tasks,
        name = "get_failed_tasks",
        description = "Получить информацию о задачах из планировщика Windows, работа которых при прошлом запуске завершилась ошибкой. Функция вернёт название задач и код ошибки из планировщика."
      ))
      
      # Получить CSV с задачами из планировщика
      dev_chat$register_tool(tool(
        get_tasks_csv,
        name = "get_tasks_csv",
        description = "Получить CSV таблицу с данными по задачам из панировщика заданий Windows. С их описанием и всеми параметрами. Эту функцию удобно использовать для поиска нужных задач по запросу пользователя."
      ))
      
      # Запустить задачу на сервере
      dev_chat$register_tool(tool(
        run_server_task,
        name = "run_server_task",
        description = "Запуск задачи в планировщике заданий Windows на сервере. Важно при передаче аргумента task_name не добавлять в назании задачь лищние слешы.",
        arguments = list(
          task_name = type_string(
            "Название задачи из планировщика заданий Windows которую надо запустить на сервере."
          )
        )
      ))
      
      # Получить данные по задаче из Планфикс
      dev_chat$register_tool(tool(
        find_pf_task_data,
        name = "find_pf_task_data",
        description = "Получить данные о задаче из Планфикс, описание задачи хранится в поле description. Данные возвращаются в JSON формате.",
        arguments = list(
          task_link = type_string(
            "Ссылка на задачу в Планфикс."
          )
        )
      ))
      
      # Активировать или деактивировать задачу
      dev_chat$register_tool(tool(
        task_state_change,
        name = "task_state_change",
        description = "Активировать (включать) и деактивировать (выключать) залачи в планировщике заданий Windows, изменяет параметр Scheduled Task State.",
        arguments = list(
          task_name = type_string(
            "Название задачи из планировщика заданий Windows по которой надо получить лог выполнения (Rout файл) запускаемого скрипта"
          ),
          action = type_string(
            "Какое действие надо выполнить с задачей, Enable - активировать (включить) задачу, Disable - деактивировать (отключить) заадчу."
          )
        )
      ))
      
      # Задачи аналитиков в Планфиксе
      dev_chat$register_tool(tool(
        get_sprint_planfix_tasks,
        name = "get_sprint_planfix_tasks",
        description = "Функция которая позволяет запросить задачи из Планфик (не из планировщика заданий Windows!) команды аналитиков, для анализа их спринта и успешности выполнения ими задач. По умолчанию возвращаются данные за текущий спринт.",
        arguments = list(
          year = type_string("Год в формате двухзначного числа (format(Sys.Date(), '%y')) если 2025 год то 25, если 2024 то 24, и так далее, по умолчанию берётся текущий год."),
          month = type_string("Месяц в формате двухзначного числа (format(Sys.Date(), '%m')) если январь то 01, если май то 05, и так далее, по умолчанию берётся текущий месяц.")
        )
      ))
      
      # Юнит-экономика
      dev_chat$register_tool(tool(
        unit_economics_analytics,
        name = "unit_economics_analytics",
        description = "Функция для получения данных для расчёта дохода по Юнит Экономике. Каждая строка это отдельная задача, по которой мы заработали деньги (fact_cost). И на которую потратили определённое время (fact_hours).",
        arguments = list(
          date_from = type_string("Начальная дата периода расчёта в формате YYYY-MM-DD."),
          date_to = type_string("Конечная дата периода расчёта в формате YYYY-MM-DD.")
        )
      ))
      
      # Приветвенное сообщение в чате
      observeEvent(input$simple_chat_ready, {
        req(input$simple_chat_ready)
        # быстрый вариант: сразу пробуем взять логин из модуля
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
      
      observeEvent(input$simple_chat_user_input, {
        message("Получен ввод:", input$simple_chat_user_input)
        stream <- dev_chat$stream_async(input$simple_chat_user_input)
        chat_append("simple_chat", stream)
      })
      
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
  shinyApp(ui, server, options = list(port = 81))
}
