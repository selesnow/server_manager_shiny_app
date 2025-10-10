# ========================
# Модуль поиска по файлам
# ========================

mod_tab_find_in_files_ui <- function(id) {
  ns <- NS(id)
  
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
                  
                  # --- выбор типов файлов ---
                  checkboxGroupInput(
                    ns("file_types"),
                    "Типы файлов:",
                    choices = c("R", "py", "cfg", "ini", "yaml", "yml", "json", "bat"),
                    selected = c("R", "py"),
                    inline = TRUE
                  ),
                  
                  fluidRow(
                    column(10,
                           textInput(ns("file_pattern"), NULL, placeholder = "Введите строку для поиска...", width = "100%")),
                    column(2,
                           actionButton(ns("search_btn"), "Найти", class = "btn-primary", width = "100%"))
                  ),
                  
                  # --- время обновления ---
                  uiOutput(ns("search_time_ui")),
                  
                  hr(),
                  div(style = "margin-bottom: 10px;",
                      tags$small(class = "text-muted", 
                                 icon("info-circle"), 
                                 " Кликните на строку, чтобы скопировать имя файла")),
                  DT::dataTableOutput(ns("search_results")),
                  uiOutput(ns("search_message"))
                )
            )
        )
      )
    )
  )
}

# Сервер
mod_tab_find_in_files_server <- function(id, tasks_data, auth, session_id) {
  moduleServer(id, function(input, output, session) {
    search_dirs <- c(
      'C:/my_develop_workshop',
      'C:/scripts',
      'C:/Users/Ashel/Documents',
      'C:/Users/persey/Documents'
    )
    
    search_data <- reactiveVal(NULL)
    search_time <- reactiveVal(NULL) 
    
    observeEvent(input$search_btn, {
      req(input$file_pattern, input$file_types)
      
      write_action_log(user = auth$user()$login, func = 'Find in files', session_id, value = input$file_pattern)
      
      pattern <- input$file_pattern
      extensions <- input$file_types
      
      results <- purrr::map_dfr(
        search_dirs,
        ~ {
          fif <- findInFiles(
            extensions = extensions, 
            pattern    = pattern, 
            root       = .x,
            output     = "tibble"
          ) %>% 
            as_tibble() %>% 
            mutate(file = as.character(file.path(.x, file)))
          
          if (nrow(fif) == 0) tibble() else fif
        }
      )
      
      if (nrow(results) > 0) {
        # таблица задач
        tasks <- tasks_data()
        
        results <- results %>%
          rowwise() %>%
          mutate(
            file_dir = normalizePath(dirname(file)),
            file_name = basename(file),
            'task name' = map2_chr(file_name, file_dir, possibly(\(x, y) filter(tasks, str_detect(`Task To Run`, x) & `Start In` == y) %>% pull(TaskName) %>% str_c(collapse = '\n'), NA)),
            'task state' = map2_chr(file_name, file_dir, possibly(\(x, y) filter(tasks, str_detect(`Task To Run`, x) & `Start In` == y) %>% pull(`Scheduled Task State`) %>% str_c(collapse = '\n'), NA))
          ) %>%
          ungroup() %>%
          select(-file_dir, -file_name)
      }
      
      search_data(results)
      search_time(lubridate::with_tz(Sys.time(), "Europe/Kyiv"))  # сохраняем время
      
      if (nrow(results) > 0) {
        write_find_in_files_log(
          results    = results %>% select(file, line, match) %>% mutate(extensions = str_c(extensions, collapse = ', ')),
          query      = input$file_pattern,
          user_login = auth$user()$login,
          session_id = session_id
        )
      }
      
    })
    
    # --- вывод времени поиска ---
    output$search_time_ui <- renderUI({
      time <- search_time()
      if (!is.null(time)) {
        div(
          style = "margin-top: 5px; font-size: 0.9em; color: #bbb;",
          paste("Результаты обновлены:", format(time, "%Y-%m-%d %H:%M:%S"))
        )
      }
    })
    
    output$search_results <- DT::renderDataTable({
      df <- search_data()
      req(df)
      validate(need(nrow(df) > 0, message = FALSE))
      
      df_chr <- df %>%
        mutate(
          match = format(match),
          across(-match, as.character)
        )
      
      DT::datatable(
        df_chr,
        options = list(pageLength = 10),
        rownames = FALSE,
        escape = FALSE,
        selection = 'none'
      )
    })
    
    
    output$search_message <- renderUI({
      df <- search_data()
      if (!is.null(df) && nrow(df) == 0) {
        div(
          style = "margin-top: 10px; padding: 10px; background-color: #FADBD8; border-radius: 10px;",
          strong("Файлы не найдены.")
        )
      }
    })
  })
}
