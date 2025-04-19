# UI часть модуля AI разработки
mod_tab_ai_dev_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "AI разработка",
    fluidRow(
      chat_ui("ai_chat")
    )
  )
}

# Серверная часть модуля AI разработки
mod_tab_ai_dev_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # создаём чат с системным промптом (можно заменить на твой актуальный)
    dev_chat <- ellmer::chat_gemini(
      system_prompt = paste(readLines(here::here('ai_docs', 'system_prompt.md')), collapse = "\n"), 
      echo = 'none'
    )
    
    observeEvent(input$send_btn, {
      req(input$user_input)
      
      stream <- dev_chat$stream_async(input$user_input)
      chat_append(ns("ai_chat"), stream)
    })
  })
}
