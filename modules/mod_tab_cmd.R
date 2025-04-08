# modules/mod_tab_cmd.R

mod_tab_cmd_ui <- function(id) {
  ns <- NS(id)
  tabPanel("CMD",
           fluidRow(
             column(12,
                    h3("Командная строка"),
                    uiOutput(ns("chat_ui")),
                    textInput(ns("user_input"), "Введите команду"),
                    actionButton(ns("send_btn"), "Отправить")
             )
           )
  )
}

mod_tab_cmd_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Командная строка --------------------------------------------------------
    chat_history <- reactiveVal(list())
    
    observeEvent(input$send_btn, {
      cmd <- input$user_input
      if (nzchar(cmd)) {
        # Добавим команду пользователя в чат
        new_entry <- list(sender = "Вы", text = cmd)
        history <- append(chat_history(), list(new_entry))
        
        # Выполним команду
        result <- tryCatch({
          system(cmd, intern = TRUE)
        }, error = function(e) {
          paste("Ошибка:", e$message)
        })
        
        # Добавим ответ сервера
        response_entry <- list(sender = "Сервер", text = paste(result, collapse = "\n"))
        history <- append(history, list(response_entry))
        chat_history(history)
        
        # Очистим поле ввода
        updateTextInput(session, "user_input", value = "")
      }
    }
    )
    
    output$chat_ui <- renderUI({
      history <- chat_history()
      tagList(
        lapply(history, function(msg) {
          div(
            style = paste0("margin-bottom: 10px; padding: 10px; border-radius: 10px; background-color:",
                           if (msg$sender == "Вы") "#D6EAF8" else "#D5F5E3"),
            strong(msg$sender), ": ", br(),
            tags$pre(style = "white-space: pre-wrap;", msg$text)
          )
        })
      )
    })
    
  })
}
