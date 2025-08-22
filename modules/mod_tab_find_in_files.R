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
                  uiOutput(ns("file_search_ui")),
                  
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
                           textInput(ns("file_pattern"), NULL, placeholder = "Введите строку для поиска...")),
                    column(2,
                           actionButton(ns("search_btn"), "Найти", class = "btn-primary"))
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
mod_tab_find_in_files_server <- function(id, tasks_data) {
  moduleServer(id, function(input, output, session) {
    search_dirs <- c(
      'C:/my_develop_workshop',
      'C:/scripts',
      'C:/Users/Ashel/Documents',
      'C:/Users/persey/Documents'
    )
    
    search_data <- reactiveVal(NULL)
    search_time <- reactiveVal(NULL)  # <- время поиска
    
    observeEvent(input$search_btn, {
      req(input$file_pattern, input$file_types)
      
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
            TaskName = map2_chr(file_name, file_dir, possibly(\(x, y) filter(tasks, str_detect(`Task To Run`, x) & `Start In` == y) %>% pull(TaskName) %>% str_c(collapse = '\n'), NA)),
            TaskState = map2_chr(file_name, file_dir, possibly(\(x, y) filter(tasks, str_detect(`Task To Run`, x) & `Start In` == y) %>% pull(`Scheduled Task State`) %>% str_c(collapse = '\n'), NA))
          ) %>%
          ungroup() %>%
          select(-file_dir, -file_name)
      }
      
      search_data(results)
      search_time(lubridate::with_tz(Sys.time(), "Europe/Kyiv"))  # сохраняем время
    })
    
    # UI блок "вы искали"
    output$file_search_ui <- renderUI({
      pattern <- input$file_pattern
      if (!is.null(pattern) && nzchar(pattern)) {
        div(
          style = "margin-bottom: 10px; padding: 10px; background-color: #D6EAF8; border-radius: 10px;",
          strong("Вы искали:"), br(),
          tags$pre(style = "white-space: pre-wrap;", pattern)
        )
      }
    })
    
    # --- вывод времени поиска ---
    output$search_time_ui <- renderUI({
      time <- search_time()
      if (!is.null(time)) {
        div(
          style = "margin-top: 5px; font-size: 0.9em; color: #555;",
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
        selection = 'single'
      )
    })
    
    observeEvent(input$search_results_rows_selected, {
      selected_row <- input$search_results_rows_selected
      if (!is.null(selected_row)) {
        df <- search_data()
        if (!is.null(df) && nrow(df) >= selected_row) {
          file_path <- df[selected_row, "file", drop = TRUE]
          file_name <- basename(file_path)
          
          clipr::write_clip(file_name)
          
          showNotification(
            paste("Выбран файл:", file_name),
            type = "message",
            duration = 5
          )
        }
      }
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
