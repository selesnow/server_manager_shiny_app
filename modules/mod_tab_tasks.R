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
                  # ----------- ФИЛЬТРЫ -----------
                  column(
                    width = 2,
                    div(class = "mb-3",
                        h4("Фильтры задач"),
                        uiOutput(ns("author_filter")),
                        uiOutput(ns("runas_filter")),
                        uiOutput(ns("responsible_filter")),
                        uiOutput(ns("last_result_filter")),
                        uiOutput(ns("client_filter")),
                        uiOutput(ns("task_state_filter")),
                        textOutput(ns("last_update"))
                    )
                  ),
                  # --------- Управление ----------
                  column(
                    width = 5,
                    div(class = "mb-3",
                        h4("Управление задачами"),
                        selectInput(ns("selected_task"), "Выберите задачу:",
                                    choices = NULL, width = "750px"),
                        div(class = "action-buttons",
                            uiOutput(ns("run_button")),
                            actionButton(ns("view_task_logs"),  "Логи",
                                         icon = icon("file-alt"),  class = "btn-info"),
                            actionButton(ns("analyze_log"),     "Анализ Rout",
                                         icon = icon("brain"),     class = "btn-info"),
                            actionButton(ns("view_script"),     "Код",
                                         icon = icon("code"),      class = "btn-info"),
                            actionButton(ns("analyze_script"),  "Объясни код",
                                         icon = icon("lightbulb"), class = "btn-info"),
                            actionButton(ns("view_task_readme"), "README",
                                         icon = icon("file-alt"),   class = "btn-info")
                        ),
                        div(class = "card mt-3", id = ns("task_info_card"),
                            div(class = "card-header", "Информация о задаче"),
                            div(class = "card-body",
                                uiOutput(ns("selected_task_info"))
                            )
                        )
                    )
                  ),
                  # ---------- Логи / анализ ----------
                  column(width = 5, uiOutput(ns("log_card")))
                )
            )
        )
      )
    ),
    
    # README
    fluidRow(column(12, uiOutput(ns("readme_card")))),
    
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
mod_tab_tasks_server <- function(id, all_tasks_reactive, user_role) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ────────── Реактивы ──────────
    sheet_url <- reactiveVal(NULL)
    log_content_type <- reactiveVal("text")
    show_log_card   <- reactiveVal(FALSE)
    show_readme_card <- reactiveVal(FALSE)
    script_content_reactive <- reactiveVal(NULL)
    popup_task_name <- reactiveVal(NULL)
    
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
      selectInput(ns("filter_author"), "Author:",
                  choices = sort(unique(all_tasks_reactive()$Author)),
                  multiple = TRUE)
    })
    
    output$runas_filter <- renderUI({
      req(all_tasks_reactive())
      selectInput(ns("filter_runas"), "Run As User:",
                  choices = sort(unique(all_tasks_reactive()$`Run As User`)),
                  multiple = TRUE)
    })
    
    output$responsible_filter <- renderUI({
      req(all_tasks_reactive())
      selectInput(ns("filter_responsible"), "Responsible:",
                  choices = sort(unique(all_tasks_reactive()$`Responsible`)),
                  multiple = TRUE)
    })
    
    output$last_result_filter <- renderUI({
      req(all_tasks_reactive())
      selectInput(ns("filter_last_result"), "Last Result:",
                  choices = sort(unique(all_tasks_reactive()$`Last Result`)),
                  multiple = TRUE)
    })
    
    output$client_filter <- renderUI({
      req(all_tasks_reactive())
      selectInput(ns("filter_client"), "Client:",
                  choices = sort(unique(all_tasks_reactive()$Client)),
                  multiple = TRUE)
    })
    
    # ---------- НОВЫЙ фильтр Scheduled Task State ----------
    output$task_state_filter <- renderUI({
      req(all_tasks_reactive())
      states <- sort(unique(all_tasks_reactive()$`Scheduled Task State`))
      # по умолчанию Enabled, если есть
      default_sel <- if ("Enabled" %in% states) "Enabled" else NULL
      
      selectInput(ns("filter_task_state"), "Scheduled Task State:",
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
      if (user_role() %in% c("admin", "user"))
        actionButton(ns("run_task"), "Запустить",
                     icon = icon("play"), class = "btn-success")
    })
    
    observeEvent(input$run_task, {
      req(input$selected_task)
      taskscheduler_runnow(taskname = input$selected_task)
      showNotification(glue("Задача '{input$selected_task}' запущена."),
                       type = "message")
    })
    
    observeEvent(input$run_task_popup, {
      req(input$selected_task)
      taskscheduler_runnow(taskname = popup_task_name())
      showNotification(glue("Задача '{popup_task_name()}' запущена."),
                       type = "message")
    })
    
    output$run_button <- renderUI({
      role <- user_role()
      if (role %in% c("admin", "user")) {
        actionButton(ns("run_task"), "Запустить", icon = icon("play"), class = "btn-success")
      }
    })
    
    # Рендерим карточку с логами
    output$log_card <- renderUI({
      if (show_log_card()) {
        div(class = "card",
            div(class = "card-body",
                h4(textOutput(ns("log_task_name"))),
                tabsetPanel(
                  id = ns("log_tabs"),
                  tabPanel(
                    title = tagList(icon("file-code"), "Логи"),
                    value = "logs",
                    tags$div(
                      style = "background-color: #2a2a2a; color: #ddd; padding: 10px; border-radius: 5px; max-height: 400px; overflow-y: auto;",
                      class = "light-mode-log",
                      verbatimTextOutput(ns("task_log_content"))
                    )
                  ),
                  tabPanel(
                    title = tagList(icon("chart-line"), "Анализ Rout"),
                    value = "analysis",
                    tags$div(
                      style = "background-color: #2a2a2a; color: #ddd; padding: 10px; border-radius: 5px; max-height: 400px; overflow-y: auto;",
                      class = "light-mode-log",
                      uiOutput(ns("task_log_markdown"))
                    )
                  ),
                  tabPanel(
                    title = tagList(icon("code"), "Скрипт"),
                    value = "script",
                    tags$div(
                      style = "background-color: #2a2a2a; color: #ddd; padding: 10px; border-radius: 5px; max-height: 400px; overflow-y: auto;",
                      class = "light-mode-log",
                      uiOutput(ns("task_script_markdown"))
                    )
                  ),
                  tabPanel(
                    title = tagList(icon("lightbulb"), "Анализ кода"),
                    value = "script_analysis",
                    tags$div(
                      style = "background-color: #2a2a2a; color: #ddd; padding: 10px; border-radius: 5px; max-height: 400px; overflow-y: auto;",
                      class = "light-mode-log",
                      uiOutput(ns("task_script_analysis"))
                    )
                  )
                )
            )
        )
      }
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
    
    # Рендерим карточку с README
    output$readme_card <- renderUI({
      if (show_readme_card()) {
        div(class = "card",
            div(class = "card-header", "README"),
            div(class = "card-body",
                h4(textOutput(ns("readme_task_name"))),
                tags$div(
                  style = "background-color: #2a2a2a; color: #ddd; padding: 10px; border-radius: 5px; max-height: 400px; overflow-y: auto;",
                  class = "light-mode-log",
                  uiOutput(ns("task_readme_content"))
                )
            )
        )
      }
    })
    
    # Обработчик кнопки "Логи"
    observeEvent(input$view_task_logs, {
      req(input$selected_task)
      
      # Находим выбранную задачу в таблице
      selected_task_data <- all_tasks_reactive() %>% 
        filter(TaskName == input$selected_task)
      
      if(nrow(selected_task_data) > 0) {
        # Получаем нужные поля
        task_to_run <- selected_task_data$`Task To Run`
        start_in <- selected_task_data$`Start In`
        
        # Если данные получены, вызываем функцию find_log
        if(!is.null(task_to_run) && !is.null(start_in)) {
          log_content <- try(find_log(task_to_run = task_to_run, start_in = start_in), silent = TRUE)
          
          if(inherits(log_content, "try-error")) {
            output$task_log_content <- renderText({ "Ошибка при чтении лога" })
          } else {
            output$task_log_content <- renderText({ log_content })
          }
          
          # Показываем имя задачи
          output$log_task_name <- renderText({ paste("Лог задачи:", input$selected_task) })
          
          # Активируем отображение карточки и переключаемся на вкладку логов
          show_log_card(TRUE)
          updateTabsetPanel(session, "log_tabs", selected = "logs")
        } else {
          showNotification("Не удалось найти данные для задачи", type = "error")
        }
      } else {
        showNotification("Задача не найдена", type = "error")
      }
    })
    
    # Обработчик кнопки "Код"
    observeEvent(input$view_script, {
      req(input$selected_task)
      
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
          show_readme_card(TRUE)
        } else {
          showNotification("Не удалось найти данные для задачи", type = "error")
        }
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
            easyClose = TRUE,
            footer = tagList(
              if (user_role() %in% c("admin", "user")) {
                tagList(
                  actionButton(ns("run_task_popup"), "Запустить", 
                               icon = icon("play"), 
                               class = "btn btn-success"),
                  actionButton(ns("deactivate_task_popup"), "Деактивировать", 
                               icon = icon("pause"), 
                               class = "btn btn-warning"),
                  actionButton(ns("activate_task_popup"), "Активировать", 
                               icon = icon("play-circle"), 
                               class = "btn btn-info")
                )
              },
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
