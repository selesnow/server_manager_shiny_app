# Модуль вкладки "Задачи"
library(googlesheets4)
library(googledrive)
library(glue)

# UI часть модуля
mod_tab_tasks_ui <- function(id) {
  ns <- NS(id)
  
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
                        uiOutput(ns("author_filter")),
                        uiOutput(ns("runas_filter")),
                        uiOutput(ns("last_result_filter")),
                        uiOutput(ns("client_filter")) # Добавляем фильтр по клиенту
                    )
                  ),
                  # Блок с управлением задачами
                  column(
                    width = 5,
                    div(class = "mb-3",
                        h4("Управление задачами"),
                        selectInput(ns("selected_task"), "Выберите задачу:", choices = NULL, width = '750px'),
                        div(class = "action-buttons",
                            uiOutput(ns("run_button")),
                            #actionButton(ns("run_task"), "Запустить", icon = icon("play"), class = "btn-success"),
                            actionButton(ns("view_task_logs"), "Логи", icon = icon("file-alt"), class = "btn-info"),
                            actionButton(ns("view_task_readme"), "README", icon = icon("file-alt"), class = "btn-info")
                        ),
                        # Добавляем блок информации о задаче
                        div(class = "card mt-3", id = ns("task_info_card"),
                            div(class = "card-header", "Информация о задаче"),
                            div(class = "card-body",
                                uiOutput(ns("selected_task_info"))
                            )
                        )
                    )
                  ),
                  column(
                    width = 5,
                    uiOutput(ns("log_card"))
                  )
                )
            )
        )
      )
    ),
    
    # Блок с выводом README - заменяем div на uiOutput
    fluidRow(
      column(
        width = 12,
        uiOutput(ns("readme_card"))
      )
    ),
    
    # Таблица задач
    fluidRow(
      column(
        width = 12,
        div(class = "card",
            div(class = "card-header", "Задачи"),
            div(class = "card-body",
                DTOutput(ns("task_table")),
                tagList(
                  div(class = "mt-3",
                      actionButton(ns("upload_to_gs"), "Выгрузить в Google Sheets", class = "btn btn-success"),
                      uiOutput(ns("open_gs_btn"))  # вот сюда отрендерится кнопка
                  )
                )
            )
        )
      )
    )
  )
}

# Серверная часть модуля
mod_tab_tasks_server <- function(id, all_tasks_reactive, user_role) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    sheet_url <- reactiveVal(NULL)
    
    # Реактивные значения
    task_data <- reactive({
      req(all_tasks_reactive())
      task <- all_tasks_reactive()
      
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
    
    filtered_task_names <- reactive({
      task_data()$TaskName
    })
    
    observe({
      updateSelectInput(session, "selected_task", choices = filtered_task_names())
    })
    
    # Фильтры
    output$author_filter <- renderUI({
      req(all_tasks_reactive())
      selectInput(ns("filter_author"), "Author:", choices = unique(all_tasks_reactive()$Author), multiple = TRUE)
    })
    
    output$runas_filter <- renderUI({
      req(all_tasks_reactive())
      selectInput(ns("filter_runas"), "Run As User:", choices = unique(all_tasks_reactive()$`Run As User`), multiple = TRUE)
    })
    
    output$last_result_filter <- renderUI({
      req(all_tasks_reactive())
      selectInput(ns("filter_last_result"), "Last Result:", choices = unique(all_tasks_reactive()$`Last Result`), multiple = TRUE)
    })
    
    output$client_filter <- renderUI({
      req(all_tasks_reactive())
      selectInput(ns("filter_client"), "Client:", choices = unique(all_tasks_reactive()$Client), multiple = TRUE)
    })
    
    # Таблица задач
    filtered_task_data <- reactive({
      data <- task_data()
      
      if (!is.null(input$task_search) && input$task_search != "") {
        search_term <- tolower(input$task_search)
        data <- data[apply(data, 1, function(row) any(grepl(search_term, tolower(row), fixed = TRUE))), ]
      }
      
      return(data)
    })
    
    output$task_table <- renderDT({
      datatable(filtered_task_data(), filter = "top", options = list(pageLength = 25, scrollX = TRUE))
    })
    
    # Запуск задачи
    observeEvent(input$run_task, {
      req(input$selected_task)
      taskscheduler_runnow(taskname = input$selected_task)
      showNotification(str_glue("Задача '{input$selected_task}' запущена."), type = "message")
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
            #div(class = "card-header", "Логи задачи"),
            div(class = "card-body",
                h4(textOutput(ns("log_task_name"))),
                tags$div(
                  style = "background-color: #2a2a2a; color: #ddd; padding: 10px; border-radius: 5px; max-height: 400px; overflow-y: auto;",
                  class = "light-mode-log",
                  verbatimTextOutput(ns("task_log_content"))
                )
            )
        )
      }
    })
    
    # Добавляем реактивные значения для управления видимостью карточек
    show_log_card <- reactiveVal(FALSE)
    show_readme_card <- reactiveVal(FALSE)
    
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
          
          # Активируем отображение карточки
          show_log_card(TRUE)
        } else {
          showNotification("Не удалось найти данные для задачи", type = "error")
        }
      } else {
        showNotification("Задача не найдена", type = "error")
      }
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
      shinyjs::show(id = ns("task_info_card"))
    })
    
    # Кнопка выгрузки в докс
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
    
  })
}