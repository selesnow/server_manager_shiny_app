# main_app_module.R

mainAppUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("main_ui"))
  )
}

mainAppServer <- function(id, logged_in) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$main_ui <- renderUI({
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
            tabPanel(
              title = "Службы",
              
              # Блок с управлением службами
              fluidRow(
                column(
                  width = 12,
                  div(class = "card", 
                      div(class = "card-header", "Управление службами"),
                      div(class = "card-body",
                          div(
                            h4("Выбор и управление службами"),
                            selectInput("selected_service", "Выберите службу:", choices = NULL),
                            textOutput("service_status"),
                            div(class = "action-buttons",
                                actionButton("start_service", "Запустить", icon = icon("play"), class = "btn-success"),
                                actionButton("stop_service", "Остановить", icon = icon("stop"), class = "btn-danger"),
                                actionButton("restart_service", "Перезапустить", icon = icon("sync"), class = "btn-warning")
                            )
                          )
                      )
                  )
                )
              ),
              
              # Таблица служб
              fluidRow(
                column(
                  width = 12,
                  div(class = "card",
                      div(class = "card-header", "Службы"),
                      div(class = "card-body",
                          DTOutput("service_table")
                      )
                  )
                )
              )
            ),
            
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
            tabPanel(
              title = "CMD",
              
              # Блок с управлением службами
              fluidRow(
                column(
                  width = 12,
                  div(class = "card", 
                      div(class = "card-header", "Интерфейс коммандной строки"),
                      div(class = "card-body",
                          div(
                            h4("Командная строка"),
                            uiOutput("chat_ui"),
                            fluidRow(
                              column(10,
                                     textInput("user_input", NULL, placeholder = "Введите команду...")),
                              column(2,
                                     actionButton("send_btn", "Выполнить", class = "btn-primary"))
                            )
                          )
                      )
                  )
                )
              )
            ),
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
            )
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

  })
}
