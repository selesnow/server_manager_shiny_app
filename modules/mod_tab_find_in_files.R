# Модуль поиска по файлам
# UI часть модуля
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
                  fluidRow(
                    column(10,
                           textInput(ns("file_pattern"), NULL, placeholder = "Введите строку для поиска...")),
                    column(2,
                           actionButton(ns("search_btn"), "Найти", class = "btn-primary"))
                  ),
                  hr(),
                  DT::dataTableOutput(ns("search_results")),
                  uiOutput(ns("search_message"))
                )
            )
        )
      )
    )
  )
}

# Серверная часть модуля
mod_tab_find_in_files_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Заранее определённые директории
    search_dirs <- c(
      'C:/my_develop_workshop',
      'C:/scripts',
      'C:/Users/Ashel/Documents',
      'C:/Users/persey/Documents'
    )
    
    # Состояние для хранения результатов
    search_data <- reactiveVal(NULL)
    
    observeEvent(input$search_btn, {
      req(input$file_pattern)
      pattern <- input$file_pattern
      
      results <- map_dfr(
        search_dirs,
        ~ {
          fif <- findInFiles(
            extensions = c('R', 'py'), 
            pattern    = pattern, 
            root       = .x,
            output     = "tibble"
          ) %>% 
            as_tibble() %>% 
            mutate(file = as.character(file.path(.x, file)))
          
          if (nrow(fif) == 0) {
            return(tibble())
          } else {
            return(fif)
          }
        }
      )
      
      search_data(results)
    })
    
    # UI чат-истории
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
    
    # Вывод результатов
    output$search_results <- DT::renderDataTable({
      df <- search_data()
      req(df)
      validate(need(nrow(df) > 0, message = FALSE))
      
      df_chr <- df %>%
        mutate(
          match = format(match),              # заменить as.character на format
          across(-match, as.character)        # остальные колонки — в текст
        )
      
      DT::datatable(
        df_chr,
        options = list(pageLength = 10),
        rownames = FALSE,
        escape = FALSE,
        selection = 'none'
      )
    })
    
    # Сообщение, если ничего не найдено
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
