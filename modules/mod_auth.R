# modules/mod_auth.R

mod_auth_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("auth_ui"))  # динамически отрисуем твой login form
}

mod_auth_server <- function(id, logged_in, user_role, check_user_fun) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$auth_ui <- renderUI({
      if (!logged_in()) {
        fluidPage(
          tags$head(
            tags$link(rel = "icon", type = "image/png", href = "favicon.png")
          ),
          tags$style(HTML("
            .login-container {
              display: flex;
              justify-content: center;
              align-items: center;
              height: 100vh;
              background-color: #333;
            }
            .login-container .form-container {
              width: 100%;
              max-width: 400px;
              padding: 20px;
              border: 1px solid #555;
              border-radius: 5px;
              background-color: #444;
            }
            .login-container .form-container input, 
            .login-container .form-container button {
              width: 100%;
              padding: 10px;
              margin-bottom: 10px;
              background-color: #555;
              color: #f5f5f5;
              border-color: #666;
            }
            .login-container .form-container button {
              background-color: #007bff;
            }
            #title-panel {
              text-align: center;
              color: #f5f5f5;
              font-size: 24px;
              margin-bottom: 20px;
            }
          ")),
          div(class = "login-container",
              fluidRow(
                column(12, class = "form-container",
                       tags$h2(id = "title-panel", "Авторизация"),
                       textInput(ns("login"), "Логин"),
                       passwordInput(ns("password"), "Пароль"),
                       actionButton(ns("login_btn"), "Войти"),
                       textOutput(ns("login_message"))
                )
              )
          )
        )
      }
    })
    
    observeEvent(input$login_btn, {
      res <- check_user_fun(input$login, input$password)
      if (!is.null(res)) {
        logged_in(TRUE)
        user_role(res$role[1])
      } else {
        output$login_message <- renderText("Неверный логин или пароль")
      }
    })
  })
}
