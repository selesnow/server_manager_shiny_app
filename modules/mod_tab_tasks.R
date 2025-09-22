# Модуль вкладки "Задачи"
library(googlesheets4)
library(googledrive)
library(glue)
library(shinyjs)
library(DT)
library(dplyr)

# Создаём чат для анализа Rout
chat <- ellmer::chat_google_gemini(
  system_prompt = interpolate_file(path = here::here('ai_docs', 'system_prompt.md')),
  model = "gemini-2.0-flash",
  echo  = "none"
)

# ─────────────────────────── UI ────────────────────────────
mod_tab_tasks_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Задачи",
    tags$head(
      tags$link(rel = "stylesheet",
                href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/styles/github-dark.min.css"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/highlight.min.js"),
      tags$script("hljs.highlightAll();")
    ),
    
    # двойной клик на попап
    tags$script(HTML(paste0("
    $(document).ready(function() {
      $(document).on('dblclick', '#", id, "-task_table tbody tr', function(e) {
        e.preventDefault();
        
        // Получаем TaskName из data-атрибута строки
        var taskName = $(this).attr('data-task-name');
        
        // Если data-атрибут не найден, пробуем получить из первой ячейки
        if (!taskName) {
          taskName = $(this).find('td:first').text().trim();
        }
        
        // Отправляем событие в Shiny с названием задачи вместо индекса
        Shiny.setInputValue('", id, "-task_table_cell_clicked', {
          taskName: taskName,
          value: 'dblclick',
          timestamp: new Date().getTime()
        }, {priority: 'event'});
      });
    });
  "))),
    
    # ----- Блок фильтров и управления -----
    fluidRow(
      column(
        width = 12,
        div(class = "card",
            div(class = "card-header", "Фильтры и управление задачами"),
            div(class = "card-body",
                fluidRow(
                  column(
                    width = 12,
                    div(class = "mb-3",
                        h4("Фильтры задач")
                    )
                  )
                ),
                fluidRow(
                  # ----------- ФИЛЬТРЫ -----------
                  column(width = 2, class = "mb-3", uiOutput(ns("author_filter"))),
                  column(width = 2, class = "mb-3", uiOutput(ns("runas_filter"))),
                  column(width = 2, class = "mb-3", uiOutput(ns("responsible_filter"))),
                  column(width = 2, class = "mb-3", uiOutput(ns("last_result_filter"))),
                  column(width = 2, class = "mb-3", uiOutput(ns("client_filter"))),
                  column(width = 2, class = "mb-3", uiOutput(ns("task_state_filter")))
                ),
                fluidRow(
                  # --------- Управление ----------
                  column(
                    width = 5,
                    div(
                      class = "d-flex flex-column h-100",
                      
                      # --- Управление задачами ---
                      div(
                        class = "mb-3",
                        h4("Управление задачами"),
                        selectInput(
                          ns("selected_task"), "Выберите задачу:",
                          choices = NULL, width = "750px"
                        ),
                        div(
                          class = "action-buttons",
                          style = "padding: 10px; background-color: #343a40; border-radius: 8px; width: 100%; margin: 0 auto;",
                          
                          # Стили для кнопок
                          tags$style("
                            .action-buttons { text-align: center; width: 100%; }
                            .action-buttons .btn-row {
                              display: flex !important;
                              justify-content: center !important;
                              align-items: center !important;
                              gap: 6px;
                              margin-bottom: 8px;
                              flex-wrap: wrap;
                              width: 100%;
                            }
                            .action-buttons .btn-row:last-child { margin-bottom: 0; }
                            .action-buttons .btn {
                              font-size: 12px !important;
                              padding: 5px 10px !important;
                              height: 34px !important;
                              min-width: 115px !important;
                              max-width: 140px !important;
                              white-space: nowrap !important;
                              overflow: hidden !important;
                              text-overflow: ellipsis !important;
                            }
                            @media (max-width: 768px) {
                              .action-buttons .btn {
                                min-width: 90px !important;
                                max-width: 100px !important;
                                font-size: 11px !important;
                                padding: 4px 8px !important;
                                height: 32px !important;
                              }
                              .action-buttons .btn-row { gap: 4px; }
                            }
                            @media (max-width: 480px) {
                              .action-buttons .btn {
                                min-width: 70px !important;
                                max-width: 75px !important;
                                font-size: 10px !important;
                                padding: 3px 6px !important;
                                height: 30px !important;
                              }
                              .action-buttons .btn-row { gap: 3px; }
                            }
                          "),
                          
                          # --- Ряд 1: Основные действия ---
                          div(class = "btn-row",
                              uiOutput(ns("run_button")),
                              uiOutput(ns("activate_task")),
                              uiOutput(ns("deactivate_task"))
                          ),
                          
                          # --- Ряд 2: Дополнительные функции ---
                          div(class = "btn-row",
                              actionButton(ns("view_task_logs"), "Логи", icon = icon("file-alt"), class = "btn-info"),
                              uiOutput(ns('analyze_log_button')),
                              actionButton(ns("view_script"), "Код", icon = icon("code"), class = "btn-info"),
                              uiOutput(ns('analyze_script_button')),
                              actionButton(ns("view_task_readme"), "README", icon = icon("book"), class = "btn-info"),
                              actionButton(ns("view_task_news"), "NEWS", icon = icon("newspaper"), class = "btn-info")
                          )
                        )
                      ),
                      
                      # --- Информация о задаче ---
                      div(
                        class = "card mt-3 flex-grow-1",
                        id = ns("task_info_card"),
                        div(class = "card-header", "Информация о задаче"),
                        div(class = "card-body",
                            uiOutput(ns("selected_task_info"))
                        )
                      ),
                      div(class = "mb-3", style = "margin-top: 5px; font-size: 0.9em; color: #bbb;",
                          textOutput(ns("last_update"))
                      )
                    )
                  ),
                  # ---------- Логи / анализ ----------
                  column(
                    width = 7,
                    div(
                      class = "card h-100 d-flex flex-column",
                      uiOutput(ns("log_card"))
                    )
                  )
                )
            )
        )
      )
    ),
    
    # ----- Таблица задач -----
    fluidRow(
      column(
        width = 12,
        div(class = "card",
            div(class = "card-header", "Задачи", tags$small(class = "text-muted ms-2", "(двойной клик по строке для подробной информации)")),
            div(class = "card-body",
                DTOutput(ns("task_table")),
                div(class = "mt-3",
                    actionButton(ns("upload_to_gs"), "Выгрузить в Google Sheets",
                                 class = "btn btn-success"),
                    uiOutput(ns("open_gs_btn"))
                )
            )
        )
      )
    )
  )
}

# ─────────────────────── SERVER ────────────────────────────
mod_tab_tasks_server <- function(id, all_tasks_reactive, task_triggers_data, user_role, auth, session_id, conf_rv) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ────────── Реактивы ──────────
    sheet_url <- reactiveVal(NULL)
    log_content_type <- reactiveVal("text")
    show_log_card   <- reactiveVal(TRUE)
    script_content_reactive <- reactiveVal(NULL)
    popup_task_name <- reactiveVal(NULL)
    current_task_start_in <- reactiveVal(NULL)
    log_history_files <- reactiveVal(NULL)
    current_log_content <- reactiveVal(NULL)
    
    # --- Главная реактивка с учётом всех фильтров ---
    task_data <- reactive({
      req(all_tasks_reactive())
      task <- all_tasks_reactive()
      
      if (!is.null(input$filter_author))
        task <- task %>% filter(Author %in% input$filter_author)
      if (!is.null(input$filter_runas))
        task <- task %>% filter(`Run As User` %in% input$filter_runas)
      if (!is.null(input$filter_last_result))
        task <- task %>% filter(`Last Result` %in% input$filter_last_result)
      if (!is.null(input$filter_responsible))
        task <- task %>% filter(`Responsible` %in% input$filter_responsible)
      if (!is.null(input$filter_client))
        task <- task %>% filter(Client %in% input$filter_client)
      if (!is.null(input$filter_task_state))
        task <- task %>% filter(`Scheduled Task State` %in% input$filter_task_state)
      
      task
    })
    
    # Для selectInput задач
    observe({
      updateSelectInput(session, "selected_task",
                        choices = task_data()$TaskName)
    })
    
    # ──────────── UI‑фильтры ────────────
    output$author_filter <- renderUI({
      req(all_tasks_reactive())
      selectInput(ns("filter_author"), "Автор:",
                  choices = sort(unique(all_tasks_reactive()$Author)),
                  multiple = TRUE)
    })
    
    output$runas_filter <- renderUI({
      req(all_tasks_reactive())
      selectInput(ns("filter_runas"), "Под какой учёткой запускается:",
                  choices = sort(unique(all_tasks_reactive()$`Run As User`)),
                  multiple = TRUE)
    })
    
    output$responsible_filter <- renderUI({
      req(all_tasks_reactive())
      selectInput(ns("filter_responsible"), "Ответственный:",
                  choices = sort(unique(all_tasks_reactive()$`Responsible`)),
                  multiple = TRUE)
    })
    
    output$last_result_filter <- renderUI({
      req(all_tasks_reactive())
      selectInput(ns("filter_last_result"), "Результат последнего запуска:",
                  choices = sort(unique(all_tasks_reactive()$`Last Result`)),
                  multiple = TRUE)
    })
    
    output$client_filter <- renderUI({
      req(all_tasks_reactive())
      selectInput(ns("filter_client"), "Клиент:",
                  choices = sort(unique(all_tasks_reactive()$Client)),
                  multiple = TRUE)
    })
    
    # ---------- НОВЫЙ фильтр Scheduled Task State ----------
    output$task_state_filter <- renderUI({
      req(all_tasks_reactive())
      states <- sort(unique(all_tasks_reactive()$`Scheduled Task State`))
      # по умолчанию Enabled, если есть
      default_sel <- if ("Enabled" %in% states) "Enabled" else NULL
      
      selectInput(ns("filter_task_state"), "Состояние задачи:",
                  choices   = states,
                  multiple  = TRUE,
                  selected  = default_sel)
    })
    
    # ───── Информация про время обновления данных ─────
    output$last_update <- renderText({
      data <- task_data()
      req(data)
      
      last_time <- max(data$update_time, na.rm = TRUE)
      paste0("Данные обновлены: ", format(last_time, "%Y-%m-%d %H:%M:%S %Z"))
    })
    
    
    # ───── Таблица с поиском по всем столбцам ─────
    output$task_table <- renderDT({
      df <- task_data() %>% select(-update_time)
      
      datatable(df,
                filter   = "top",
                options  = list(
                  pageLength = 25, 
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  # Добавляем callback для каждой строки
                  rowCallback = JS(
                    "function(row, data, index) {",
                    "  // Добавляем TaskName как data-атрибут к строке",
                    "  $(row).attr('data-task-name', data[0]);", # data[0] - первый столбец (TaskName)
                    "}"
                  )
                ),
                selection = 'none') %>%
        formatStyle(columns = 1:ncol(df), cursor = 'pointer')
    })
    
    # ───────────── КНОПКИ и обработчики ─────────────
    output$run_button <- renderUI({
      if (user_role() %in% conf_rv()$access_managemet$`Запуск задач`)
        actionButton(ns("run_task"), "Запустить", icon = icon("play"), class = "btn-success")
    })
    
    output$analyze_log_button <- renderUI({
      if (user_role() %in% conf_rv()$access_managemet$`AI анализ`)
        actionButton(ns("analyze_log"), "Анализ Rout", icon = icon("brain"), class = "btn-info")
    })
    
    output$analyze_script_button <- renderUI({
      if (user_role() %in% conf_rv()$access_managemet$`AI анализ`)
        actionButton(ns("analyze_script"), "Объясни код", icon = icon("lightbulb"), class = "btn-info")
    })
    
    observeEvent(input$run_task, {
      req(input$selected_task)
      write_action_log(user = auth$user()$login, func = 'Task run', session_id, value = input$selected_task)
      taskscheduler_runnow(taskname = input$selected_task)
      showNotification(glue("Задача '{input$selected_task}' запущена."),
                       type = "message")
    })
    
    # Активировать задачу (блок управления задачами)
    observeEvent(input$activate_task, {
      req(input$selected_task)
      
      write_action_log(user = auth$user()$login, func = 'Activate task',
                       session_id, value = input$selected_task)
      
      task_state_change(input$selected_task, action = "Enable")
      showNotification(paste("Задача", input$selected_task, "активирована"), type = "message")
    })
    
    # Деактивировать задачу (блок управления задачами)
    observeEvent(input$deactivate_task, {
      write_action_log(user = auth$user()$login, func = 'Deactivate task',
                       session_id, value = input$selected_task)
      req(input$selected_task)
      
      task_state_change(input$selected_task, action = "Disable")
      showNotification(paste("Задача", input$selected_task, "деактивирована"), type = "warning")
    })
    
    
    observeEvent(input$run_task_popup, {
      req(input$selected_task)
      write_action_log(user = auth$user()$login, func = 'Task run (popup)', session_id, value = popup_task_name())
      taskscheduler_runnow(taskname = popup_task_name())
      showNotification(glue("Задача '{popup_task_name()}' запущена."),
                       type = "message")
    })
    
    output$run_button <- renderUI({
      role <- user_role()
      if (role %in% conf_rv()$access_managemet$`Запуск задач`) {
        actionButton(ns("run_task"), "Запустить", icon = icon("play"), class = "btn-success")
      }
    })
    
    output$activate_task <- renderUI({
      role <- user_role()
      if (role %in% conf_rv()$access_managemet$`Активация задач`) {
        actionButton(ns("activate_task"), "Активировать",
                     icon = icon("toggle-on"), class = "btn-success")
      }
    })
    
    output$deactivate_task <- renderUI({
      role <- user_role()
      if (role %in% conf_rv()$access_managemet$`Активация задач`) {
        actionButton(ns("deactivate_task"), "Деактивировать",
                     icon = icon("toggle-off"), class = "btn-danger btn-sm ml-2")
      }
    })
    
    # Определяем, показывать ли историю логов
    output$show_log_history <- reactive({
      !is.null(log_history_files())
    })
    outputOptions(output, "show_log_history", suspendWhenHidden = FALSE)
    
    # Рендерим карточку с логами
    output$log_card <- renderUI({
        div(class = "card",
            div(class = "card-body",
                #h4(textOutput(ns("log_task_name"))),
                tabsetPanel(
                  id = ns("log_tabs"),
                  tabPanel(
                    title = tagList(icon("file-code"), "Логи"),
                    value = "logs",
                    # НОВЫЙ блок с выбором лога
                    conditionalPanel(
                      condition = paste0("output['", ns("show_log_history"), "']"),
                      div(class = "mb-3",
                          div(class = "row",
                              div(class = "col-md-8",
                                  selectInput(ns("selected_log_file"), 
                                              "Выберите лог:", 
                                              choices = NULL,
                                              width = "100%")
                              ),
                              div(class = "col-md-4",
                                  actionButton(ns("load_current_log"), 
                                               "Текущий лог", 
                                               class = "btn btn-primary btn-sm",
                                               style = "margin-top: 25px;")
                              )
                          )
                      )
                    ),
                    tags$div(
                      style = "background-color: #2a2a2a; color: #ddd; padding: 10px; border-radius: 5px; max-height: 600px; overflow-y: auto;",
                      class = "light-mode-log",
                      verbatimTextOutput(ns("task_log_content"))
                    )
                  ),
                  tabPanel(
                    title = tagList(icon("chart-line"), "Анализ Rout"),
                    value = "analysis",
                    tags$div(
                      style = "background-color: #2a2a2a; color: #ddd; padding: 10px; border-radius: 5px; max-height: 600px; overflow-y: auto;",
                      class = "light-mode-log",
                      uiOutput(ns("task_log_markdown"))
                    )
                  ),
                  tabPanel(
                    title = tagList(icon("code"), "Скрипт"),
                    value = "script",
                    tags$div(
                      style = "background-color: #2a2a2a; color: #ddd; padding: 10px; border-radius: 5px; max-height: 600px; overflow-y: auto;",
                      class = "light-mode-log",
                      uiOutput(ns("task_script_markdown"))
                    )
                  ),
                  tabPanel(
                    title = tagList(icon("lightbulb"), "Анализ кода"),
                    value = "script_analysis",
                    tags$div(
                      style = "background-color: #2a2a2a; color: #ddd; padding: 10px; border-radius: 5px; max-height: 600px; overflow-y: auto;",
                      class = "light-mode-log",
                      uiOutput(ns("task_script_analysis"))
                    )
                  ),
                  tabPanel(
                    title = tagList(icon("book"), "README"),
                    value = "readme",
                    tags$div(
                      style = "background-color: #2a2a2a; color: #ddd; padding: 10px; border-radius: 5px; max-height: 600px; overflow-y: auto;",
                      class = "light-mode-log",
                      uiOutput(ns("task_readme_content"))
                    )
                  ),
                  tabPanel(
                    title = tagList(icon("newspaper"), "NEWS"),
                    value = "news",
                    tags$div(
                      style = "background-color: #2a2a2a; color: #ddd; padding: 10px; 
                               border-radius: 5px; max-height: 600px; overflow-y: auto;",
                      class = "light-mode-log",
                      uiOutput(ns("task_news_content"))
                    )
                  )
                )
            )
        )
    })
    
    # Добавляем реактивные значения для управления видимостью карточек
    show_log_card <- reactiveVal(FALSE)
    show_readme_card <- reactiveVal(FALSE)
    script_content_reactive <- reactiveVal(NULL)
    
    # Следим за изменениями вкладки
    observeEvent(input$log_tabs, {
      if (input$log_tabs == "script") {
        # Запускаем JavaScript-функцию для инициализации поиска
        shinyjs::runjs("if(window.reinitScriptSearch) window.reinitScriptSearch();")
      }
    })
    
    # Обработчик кнопки "Логи"
    # ОБНОВЛЕННЫЙ обработчик кнопки "Логи"
    observeEvent(input$view_task_logs, {
      req(input$selected_task)
      
      write_action_log(user = auth$user()$login, func = 'Task log', session_id, value = input$selected_task)
      
      selected_task_data <- all_tasks_reactive() %>% 
        filter(TaskName == input$selected_task)
      
      if(nrow(selected_task_data) > 0) {
        task_to_run <- selected_task_data$`Task To Run`
        start_in <- selected_task_data$`Start In`
        
        current_task_start_in(start_in)
        
        # Проверяем наличие истории логов
        history_files <- get_log_history_files(start_in, task_to_run)
        log_history_files(history_files)
        
        # Обновляем выпадающий список если есть история
        if (!is.null(history_files)) {
          updateSelectInput(session, "selected_log_file", 
                            choices = c("Текущий лог" = "current", history_files))
        }
        
        if(!is.null(task_to_run) && !is.null(start_in)) {
          # Загружаем текущий лог по умолчанию
          log_content <- try(find_log(task_to_run = task_to_run, start_in = start_in), silent = TRUE)
          
          if(inherits(log_content, "try-error")) {
            current_log_content("Ошибка при чтении лога")
          } else {
            current_log_content(log_content)
          }
          
          output$task_log_content <- renderText({ current_log_content() })
          output$log_task_name <- renderText({ paste("Лог задачи:", input$selected_task) })
          
          show_log_card(TRUE)
          updateTabsetPanel(session, "log_tabs", selected = "logs")
        } else {
          showNotification("Не удалось найти данные для задачи", type = "error")
        }
      } else {
        showNotification("Задача не найдена", type = "error")
      }
    })
    
    # НОВЫЙ обработчик для кнопки "Текущий лог"
    observeEvent(input$load_current_log, {
      req(input$selected_task, current_task_start_in())
      
      selected_task_data <- all_tasks_reactive() %>% 
        filter(TaskName == input$selected_task)
      
      if(nrow(selected_task_data) > 0) {
        task_to_run <- selected_task_data$`Task To Run`
        start_in <- current_task_start_in()
        
        if(!is.null(task_to_run) && !is.null(start_in)) {
          log_content <- try(find_log(task_to_run = task_to_run, start_in = start_in), silent = TRUE)
          
          if(inherits(log_content, "try-error")) {
            current_log_content("Ошибка при чтении текущего лога")
          } else {
            current_log_content(log_content)
          }
          
          output$task_log_content <- renderText({ current_log_content() })
          updateSelectInput(session, "selected_log_file", selected = "current")
        }
      }
    })
    
    # НОВЫЙ обработчик для выбора лога из истории
    observeEvent(input$selected_log_file, {
      
      req(input$selected_log_file)
      
      if (input$selected_log_file == "current") {
        # Если выбран текущий лог, вызываем стандартную загрузку
        req(input$selected_task, current_task_start_in())
        
        selected_task_data <- all_tasks_reactive() %>% 
          filter(TaskName == input$selected_task)
        
        if(nrow(selected_task_data) > 0) {
          task_to_run <- selected_task_data$`Task To Run`
          start_in <- current_task_start_in()
          
          if(!is.null(task_to_run) && !is.null(start_in)) {
            log_content <- try(find_log(task_to_run = task_to_run, start_in = start_in), silent = TRUE)
            
            if(inherits(log_content, "try-error")) {
              current_log_content("Ошибка при чтении текущего лога")
            } else {
              current_log_content(log_content)
            }
          }
        }
      } else {
        # Если выбран исторический лог
        if (file.exists(input$selected_log_file)) {
          log_content <- try({
            readLines(input$selected_log_file, warn = FALSE) %>% 
              str_c(collapse = '\n')
          }, silent = TRUE)
          
          if(inherits(log_content, "try-error")) {
            current_log_content("Ошибка при чтении исторического лога")
          } else {
            current_log_content(log_content)
          }
        } else {
          current_log_content("Файл лога не найден")
        }
      }
      
      output$task_log_content <- renderText({ current_log_content() })
    })
    
    # Обработчик кнопки "Код"
    observeEvent(input$view_script, {
      req(input$selected_task)
      write_action_log(user = auth$user()$login, func = 'Task script', session_id, value = input$selected_task)
      selected_task_data <- all_tasks_reactive() %>% 
        filter(TaskName == input$selected_task)
      
      if (nrow(selected_task_data) > 0) {
        task_to_run <- selected_task_data$`Task To Run`
        start_in    <- selected_task_data$`Start In`
        
        if (!is.null(task_to_run) && !is.null(start_in)) {
          script_content <- try(find_script(task_to_run = task_to_run, start_in = start_in), silent = TRUE)
          
          if (inherits(script_content, "try-error") || is.null(script_content)) {
            script_content_reactive("Ошибка при чтении скрипта")
          } else {
            script_content_reactive(script_content)
          }
          
          output$log_task_name <- renderText({ paste("Скрипт задачи:", input$selected_task) })
          show_log_card(TRUE)
          updateTabsetPanel(session, "log_tabs", selected = "script")
          
          # Реинициализация подсветки кода через highlight.js
          shinyjs::runjs("setTimeout(function() { hljs.highlightAll(); }, 500);")
        }
      }
    })
    
    # Рендер скрипта на вкладке "Скрипт"
    output$task_script_markdown <- renderUI({
      req(script_content_reactive())
      
      script_text <- script_content_reactive()
      
      HTML(markdown::markdownToHTML(
        text = paste0("```r\n", script_text, "\n```"),
        fragment.only = TRUE
      ))
    })
    
    # Обработчик кнопки "README"
    observeEvent(input$view_task_readme, {
      req(input$selected_task)
      write_action_log(user = auth$user()$login, func = 'Task README', session_id, value = input$selected_task)
      # Находим выбранную задачу в таблице
      selected_task_data <- all_tasks_reactive() %>% 
        filter(TaskName == input$selected_task)
      
      if(nrow(selected_task_data) > 0) {
        # Получаем нужные поля
        start_in <- selected_task_data$`Start In`
        
        # Если данные получены, вызываем функцию find_readme
        if(!is.null(start_in)) {
          
          # очищаем от папки R
          start_in <- str_remove(start_in, '\\\\R$|/R$|/R/$')
          readme_content <- try(find_readme(start_in = start_in), silent = TRUE)
          
          output$task_readme_content <- renderUI({
            md_path   <- file.path(unique(start_in), "README.md")
            html_path <- file.path(unique(start_in), "README.html")
            
            if (file.exists(md_path)) {
              includeMarkdown(md_path)
            } else if (file.exists(html_path)) {
              includeHTML(html_path)
            } else {
              HTML("<p>README не найден!</p>")
            }
          })
          
          # Показываем имя задачи
          output$readme_task_name <- renderText({ paste("README:", input$selected_task) })
          
          # Активируем отображение карточки
          output$log_task_name <- renderText({ paste("README задачи:", input$selected_task) })
          show_log_card(TRUE)
          updateTabsetPanel(session, "log_tabs", selected = "readme")
        } else {
          showNotification("Не удалось найти данные для задачи", type = "error")
        }
      } else {
        showNotification("Задача не найдена", type = "error")
      }
    })
    
    # NEWS
    observeEvent(input$view_task_news, {
      req(input$selected_task)
      write_action_log(user = auth$user()$login, func = 'Task NEWS', session_id, value = input$selected_task)
      
      selected_task_data <- all_tasks_reactive() %>% 
        filter(TaskName == input$selected_task)
      
      if (nrow(selected_task_data) > 0) {
        start_in <- selected_task_data$`Start In`
        start_in <- str_remove(start_in, '\\\\R$|/R$|/R/$')
        
        news_content <- try(find_news(start_in = start_in), silent = TRUE)
        
        output$task_news_content <- renderUI({
          md_path   <- file.path(unique(start_in), "NEWS.md")
          html_path <- file.path(unique(start_in), "NEWS.html")
          
          if (file.exists(md_path)) {
            includeMarkdown(md_path)
          } else if (file.exists(html_path)) {
            includeHTML(html_path)
          } else {
            HTML("<p>NEWS не найден!</p>")
          }
        })
        
        output$log_task_name <- renderText({ paste("NEWS:", input$selected_task) })
        show_log_card(TRUE)
        updateTabsetPanel(session, "log_tabs", selected = "news")
      } else {
        showNotification("Задача не найдена", type = "error")
      }
    })
    
    
    # Вывод дополнительный информации о выбранной задаче
    selected_task_details <- reactive({
      req(input$selected_task)
      all_tasks_reactive() %>% 
        filter(TaskName == input$selected_task) %>%
        select(
          TaskName, 
          Author, 
          `Run As User`, 
          `Start In`, 
          `Task To Run`, 
          Client, 
          Comment, 
          `Last Run Time`, 
          `Last Result`, 
          `Scheduled Task State`,
          Responsible,
          readme, news, git, rproj, has_log
          )
    })
    
    # Рендинг триггеров
    output$task_triggers_table <- gt::render_gt({
      req(task_triggers_data(), input$selected_task)
      
      triggers <- task_triggers_data() |>
        dplyr::filter(task_name == input$selected_task)
      
      if (nrow(triggers) == 0) {
        gt::gt(data.frame(Сообщение = "Триггеры не найдены")) |> 
          gt::tab_options(table.align = "center",
                          table.background.color = "transparent")
      } else {
        triggers |>
          select(-task_name) |>
          mutate(schedule_details = purrr::map(schedule_details, gt::html)) |>
          gt::gt() |>
          gt::tab_style(
            style = gt::cell_text(color = "#e0e0e0"),
            locations = gt::cells_body()
          ) |>
          gt::tab_style(
            style = gt::cell_text(color = "#e0e0e0"),
            locations = gt::cells_column_labels()
          ) |>
          gt::tab_options(
            table.font.size = "small",
            data_row.padding = gt::px(2),
            table.background.color = "transparent"
          )
      }
    })
    
    # Рендерим информацию о выбранной задаче
    output$selected_task_info <- renderUI({
      req(selected_task_details())
      task_info <- selected_task_details()
      
      if(nrow(task_info) > 0) {
        task <- task_info[1, ]
        div(
          div(class = "mb-2",
              strong("Название: "),
              span(id = ns("task_name_text"), input$selected_task, style = "cursor: pointer;"),
              actionButton(ns("copy_task_name"), label = NULL, icon = icon("copy"), class = "btn btn-sm btn-outline-secondary", style = "margin-left: 5px;")
          ),
          div(class = "mb-2", strong("Автор: "), span(task$Author)),
          div(class = "mb-2", strong("Запускается от имени: "), span(task$`Run As User`)),
          div(class = "mb-2", strong("Ответственный: "), span(task$`Responsible`)),
          div(class = "mb-2", strong("Статус задачи: "), span(task$`Scheduled Task State`)),
          div(class = "mb-2", strong("Директория: "), span(task$`Start In`)),
          div(class = "mb-2", strong("Команда запуска: "), span(task$`Task To Run`)),
          div(class = "mb-2", strong("Время прошлого запуска: "), span(task$`Last Run Time`)),
          div(class = "mb-2", strong("Результат прошлого запуска: "), span(task$`Last Result`)),
          div(class = "mb-2", strong("Клиент: "), span(task$Client)),
          div(class = "mb-2", strong("Краткое описание: "), span(task$Comment)),
          br(),
          # Триггеры
          div(class = "mb-3",
              strong("Триггеры:"),
              gt::gt_output(ns("task_triggers_table"))
          ),
          # Новая строка с индикаторами
          div(class = "mb-2", 
              strong("Наличие элементов проекта: "),
              br(),
              div(style = "margin-top: 5px;",
                  create_indicator("readme", task$readme, "README"),
                  create_indicator("news", task$news, "NEWS"),
                  create_indicator("git", task$git, "Git"),
                  create_indicator("rproj", task$rproj, "Rproj"),
                  create_indicator("has_log", task$has_log, "Log")
              )
          )
        )
      } else {
        div("Информация недоступна")
      }
    })
    
    # копирование название задачи
    observeEvent(input$copy_task_name, {
      task_name_escaped <- gsub("\\\\", "\\\\\\\\", input$selected_task)
      js_code <- sprintf("navigator.clipboard.writeText('%s')", task_name_escaped)
      shinyjs::runjs(js_code)
      showNotification("Название задачи скопировано", type = "message")
    })
    
    observeEvent(input$task_name_copied, {
      showNotification("Название задачи скопировано", type = "message")
    })
    
    # Показываем информацию при выборе задачи
    observeEvent(input$selected_task, {
      shinyjs::show(id = ns("task_info_card"))
    })
    
    # Кнопка выгрузки в докс
    filtered_task_data <- reactive({
      data <- task_data()
      
      if (!is.null(input$task_search) && input$task_search != "") {
        search_term <- tolower(input$task_search)
        data <- data[apply(data, 1, function(row) any(grepl(search_term, tolower(row), fixed = TRUE))), ]
      }
      
      return(data)
    })
    
    observeEvent(input$upload_to_gs, {
      req(filtered_task_data())

      # Авторизация
      gs4_auth(path = serviceaccounts::get_sa_from_internal_file())
      drive_auth(path = serviceaccounts::get_sa_from_internal_file())
      
      # Создание таблицы
      ss <- gs4_create(
        glue('Task scheduller list ({Sys.time()})'),
        sheets = list(tasks = filtered_task_data())
      )
      
      # Перемещение в папку на Google Drive
      drive_mv(ss, path = "task_scheduller/")
      
      sheet_url(gs4_get(ss)$spreadsheet_url)
      
      write_action_log(user = auth$user()$login, func = 'Load task table to google sheet', session_id, value = sheet_url())
      
      # Можно тут ещё всплывающее уведомление или alert добавить
      showNotification("Файл успешно выгружен в Google Sheets!", type = "message")
    })
    
    # Кнопка открыть докс
    output$open_gs_btn <- renderUI({
      req(sheet_url())
      
      tags$a(
        href = sheet_url(),
        target = "_blank",
        class = "btn btn-primary",
        "Открыть Google Sheets"
      )
    })
    
    # Анализ Rout
    observeEvent(input$analyze_log, {
      req(input$selected_task)
      write_action_log(user = auth$user()$login, func = 'Rout ai analyze', session_id, value = input$selected_task)
      selected_task_data <- all_tasks_reactive() %>% 
        filter(TaskName == input$selected_task)
      
      if (nrow(selected_task_data) > 0) {
        task_to_run <- selected_task_data$`Task To Run`
        start_in    <- selected_task_data$`Start In`
        
        if (!is.null(task_to_run) && !is.null(start_in)) {
          log_content <- try(find_log(task_to_run = task_to_run, start_in = start_in), silent = TRUE)
          
          if (inherits(log_content, "try-error") || is.null(log_content)) {
            # Для ошибок тоже используем markdown
            output$task_log_markdown <- renderUI({ 
              HTML("<p>Ошибка при чтении лога</p>") 
            })
          } else {
            
            # Обрезаем до 30000 последних символов
            log_cut <- substr(log_content, 
                              max(1, nchar(log_content) - 29999), 
                              nchar(log_content))
            
            # Сообщение в чат Gemini
            out <- chat$chat(
              glue::glue(
                'Ниже я тебе отправлю вывод из .Rout файла моего скрипта, тебе надо его проанализировать, ',
                'и если выполнение закончилось ошибкой, то дать описание чем эта ошибка вызвана и пошаговый ',
                'план по её исправлению, если выполнение скрипта выполнено успешно то напиши что скрипт был ',
                'выполнен успешно и скажи сколько минут длилось его выполнение, при успешном выполнении можешь ',
                'дать рекомендации по оптимизации скрипта.\n\n',
                'Я ограничил Rout 30000 символов, что бы не перегружать лишней информацией.',
                '## Текст Rout файла\n\n',
                '{log_cut}'
              ), echo = F
            )
            
            # Выводим результат как HTML с поддержкой markdown
            output$task_log_markdown <- renderUI({ 
              HTML(markdown::markdownToHTML(text = out, fragment.only = TRUE)) 
            })
            output$log_task_name <- renderText({ paste("Задача:", input$selected_task) })
            
            show_log_card(TRUE)
            updateTabsetPanel(session, "log_tabs", selected = "analysis")
          }
        } else {
          showNotification("Не удалось найти данные для задачи", type = "error")
        }
      } else {
        showNotification("Задача не найдена", type = "error")
      }
    })
    
    # Анализ R кода
    observeEvent(input$analyze_script, {
      req(input$selected_task)
      write_action_log(user = auth$user()$login, func = 'Script ai analyze', session_id, value = input$selected_task)
      selected_task_data <- all_tasks_reactive() %>% 
        filter(TaskName == input$selected_task)
      
      if (nrow(selected_task_data) > 0) {
        task_to_run <- selected_task_data$`Task To Run`
        start_in    <- selected_task_data$`Start In`
        
        if (!is.null(task_to_run) && !is.null(start_in)) {
          script_content <- try(find_script(task_to_run = task_to_run, start_in = start_in), silent = TRUE)
          
          if (inherits(script_content, "try-error") || is.null(script_content)) {
            output$task_script_analysis <- renderUI({ 
              HTML("<p>Ошибка при чтении скрипта</p>") 
            })
          } else {
            
            cat('Начинал анализ R кода')
            
            # Отправляем скрипт в чат Gemini
            out <- chat$chat(
              glue::glue(
                'Ниже я тебе отправлю текст скрипта на языке R.\n',
                'Твоя задача: подробно объяснить, что делает этот скрипт.\n',
                'При аналтзе R кода используй документацию о корпоративных пакетах, которую я тебе предоставил.',
                'Если в коде есть ошибки или места для оптимизации — напиши об этом в конце.\n\n',
                'Ответ начинай с саммари - пошагового оиснаия алгоритма работы скрипта, ',
                'т.е. по шагам объясни какие дейтвия выполняются.',
                'Если в код есть чтение или запись в Google таблицу выводи информцию об этом сразу с ссылкой на эту Google таблицу,',
                'если есть работа с Google BigQuery то указывай с какими именно таблицами.',
                'Не трать много времени на анализ кода, твоя задача просто объяснить что он делает.',
                '## Текст скрипта\n\n',
                '{script_content}'
              ),
              echo = FALSE
            )
            
            # Выводим результат как HTML с поддержкой markdown
            output$task_script_analysis <- renderUI({ 
              HTML(markdown::markdownToHTML(text = out, fragment.only = TRUE)) 
            })
            
            output$log_task_name <- renderText({ paste("Скрипт задачи:", input$selected_task) })
            
            show_log_card(TRUE)
            updateTabsetPanel(session, "log_tabs", selected = "script_analysis")
          }
        } else {
          showNotification("Не удалось найти данные для задачи", type = "error")
        }
      } else {
        showNotification("Задача не найдена", type = "error")
      }
    })
    
    
    # Обработчик копирования названия задачи из popup
    observeEvent(input$copy_popup_task_name, {
      req(popup_task_name())
      task_name_escaped <- gsub("\\\\", "\\\\\\\\", popup_task_name())
      js_code <- sprintf("navigator.clipboard.writeText('%s')", task_name_escaped)
      shinyjs::runjs(js_code)
      showNotification("Название задачи скопировано", type = "message")
    })
    
    # Обработчик деактивации задачи
    observeEvent(input$deactivate_task_popup, {
      req(popup_task_name())
      write_action_log(user = auth$user()$login, func = 'Task deactivate (popup)', session_id, value = popup_task_name())
      tryCatch({
        task_state_change(popup_task_name(), action = "Disable")
        showNotification(glue("Задача '{popup_task_name()}' деактивирована."), type = "message")
      }, error = function(e) {
        showNotification(paste("Ошибка при деактивации:", e$message), type = "error")
      })
    })
    
    # Обработчик активации задачи
    observeEvent(input$activate_task_popup, {
      req(popup_task_name())
      write_action_log(user = auth$user()$login, func = 'Task activate (popup)', session_id, value = popup_task_name())
      tryCatch({
        task_state_change(popup_task_name(), action = "Enable")
        showNotification(glue("Задача '{popup_task_name()}' активирована."), type = "message")
      }, error = function(e) {
        showNotification(paste("Ошибка при активации:", e$message), type = "error")
      })
    })
    
    # Окно с карточкой задачи при клике по строке задачи
    observeEvent(input$task_table_cell_clicked, {
      click_info <- input$task_table_cell_clicked
      req(click_info)
      
      # Проверяем, что это двойной клик
      if (!is.null(click_info$value) && click_info$value == "dblclick") {
        task_name <- click_info$taskName

        req(task_name)
        
        # Находим задачу по имени в отфильтрованных данных
        df <- task_data()
        row <- df %>% slice(as.numeric(task_name))
        
        if (nrow(row) > 0) {
          row <- row[1, , drop = FALSE]  # Берем первую строку если дубликаты
          
          # подхватываем название задачи
          popup_task_name(row$TaskName)
          
          showModal(modalDialog(
            title = paste("Информация о задаче:", row$TaskName),
            size = "l",
            tags$head(
              tags$style(HTML("
              .modal-content {
                  background-color: #2b2b2b !important;   /* тёмный фон */
                  color: #f0f0f0 !important;              /* светлый текст */
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
              @media (max-width: 768px) {
                .modal-dialog {
                  margin: 10px !important;
                  max-width: calc(100vw - 20px) !important;
                }
                .modal-content {
                  border-radius: 8px;
                }
                .btn {
                  margin: 2px !important;
                  font-size: 0.875rem;
                }
                .mb-2 {
                  margin-bottom: 0.75rem !important;
                }
              }
              @media (max-width: 480px) {
                .modal-dialog {
                  margin: 5px !important;
                  max-width: calc(100vw - 10px) !important;
                }
                .modal-footer .btn {
                  flex: 1;
                  margin: 1px !important;
                }
              }
              
              /* Перенос длинных строк */
              .modal-body span {
                word-break: break-word !important;
                word-wrap: break-word !important;
                white-space: normal !important;
                max-width: 100%;
                display: inline-block;
              }
              
              /* Специально для путей и команд */
              .modal-body .mb-2 {
                word-break: break-all !important;
              }
              
              /* На мобильных делаем более агрессивный перенос */
              @media (max-width: 768px) {
                .modal-body span {
                  word-break: break-all !important;
                  hyphens: auto;
                }
                .modal-body div.mb-2 {
                  margin-bottom: 0.5rem !important;
                }
              }
            "))
            ),
            easyClose = TRUE,
            footer = tagList(
              tagList(
                if (user_role() %in% conf_rv()$access_managemet$`Запуск задач`) {
                  actionButton(ns("run_task_popup"), "Запустить", 
                               icon = icon("play"), 
                               class = "btn btn-success")
                },
                if (user_role() %in% conf_rv()$access_managemet$`Активация задач`) {
                  actionButton(ns("activate_task_popup"), "Активировать", 
                               icon = icon("toggle-on"), 
                               class = "btn-success")
                },
                if (user_role() %in% conf_rv()$access_managemet$`Активация задач`) {
                  actionButton(ns("deactivate_task_popup"), "Деактивировать", 
                               icon = icon("toggle-off"), 
                               class = "btn-danger btn-sm ml-2")
                }
              ),
              modalButton("Закрыть")
            ),
            div(
              div(class = "mb-2",
                  strong("Название: "),
                  span(id = ns("popup_task_name_text"), row$TaskName, style = "cursor: pointer;"),
                  actionButton(ns("copy_popup_task_name"), label = NULL, icon = icon("copy"), 
                               class = "btn btn-sm btn-outline-secondary", style = "margin-left: 5px;")
              ),
              div(class = "mb-2", strong("Автор: "),              span(row$Author)),
              div(class = "mb-2", strong("Запускается от имени: "), span(row$`Run As User`)),
              div(class = "mb-2", strong("Ответственный: "),      span(row$Responsible)),
              div(class = "mb-2", strong("Статус задачи: "),      span(row$`Scheduled Task State`)),
              div(class = "mb-2", strong("Директория: "),         span(row$`Start In`)),
              div(class = "mb-2", strong("Команда запуска: "),    span(row$`Task To Run`)),
              div(class = "mb-2", strong("Время прошлого запуска: "), span(row$`Last Run Time`)),
              div(class = "mb-2", strong("Результат прошлого запуска: "), span(row$`Last Result`)),
              div(class = "mb-2", strong("Клиент: "),             span(row$Client)),
              div(class = "mb-2", strong("Краткое описание: "),   span(row$Comment)),
              tags$hr(),
              div(
                class = "mb-3",
                strong("Триггеры:"),
                {
                  triggers <- task_triggers_data() |> 
                    dplyr::filter(task_name == row$TaskName)
                  
                  if (nrow(triggers) == 0) {
                    div("Триггеры не найдены")
                  } else {
                    triggers |>
                      select(-task_name) |>
                      mutate(schedule_details = purrr::map(schedule_details, gt::html)) |>
                      gt::gt() |>
                      gt::tab_style(
                        style = gt::cell_text(color = "#e0e0e0"),
                        locations = gt::cells_body()
                      ) |>
                      gt::tab_style(
                        style = gt::cell_text(color = "#e0e0e0"),
                        locations = gt::cells_column_labels()
                      ) |>
                      gt::tab_options(
                        table.font.size = "small",
                        data_row.padding = gt::px(2),
                        table.background.color = "transparent"
                      ) |> 
                      gt::as_raw_html() |> 
                      HTML() # Важно для вставки внутрь modalDialog
                  }
                }
              ),
              div(class = "mb-2",
                  strong("Наличие элементов проекта:"),
                  br(),
                  div(
                    style = "margin-top: 5px;",
                    create_indicator("readme",  isTRUE(row$readme),  "README"),
                    create_indicator("news",    isTRUE(row$news),    "NEWS"),
                    create_indicator("git",     isTRUE(row$git),     "Git"),
                    create_indicator("rproj",   isTRUE(row$rproj),   "Rproj"),
                    create_indicator("has_log", isTRUE(row$has_log), "Log")
                  )
              )
            )
          ))
          
        } else {
          showNotification("Задача не найдена", type = "error")
        }
      }
    }, ignoreInit = TRUE)
    
    
  })
}
