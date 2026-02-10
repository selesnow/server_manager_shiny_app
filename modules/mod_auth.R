# modules/mod_auth.R

mod_auth_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("auth_ui"))  # динамически отрисуем твой login form
}

mod_auth_server <- function(id, logged_in, user_role, check_user_fun) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # подхватываем логин пользователя
    user <- reactiveVal(NULL)
    
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
                       tags$img(src = "favicon.png", height = "50px", style = "display: block; margin: 0 auto 20px;"),
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
        
        # 1. фиксируем логин пользователя
        user_login <- input$login
        
        # 2. сохраняем пользователя в reactiveVal
        user(list(
          login = user_login,
          role  = res$role[1]
        ))
        
        # 3. поднимаем флаги авторизации
        logged_in(TRUE)
        user_role(res$role[1])
        
        # 4. генерим persistent token
        raw_token  <- openssl::rand_bytes(32) |> openssl::base64_encode()
        token_hash <- openssl::sha256(raw_token)
        
        # 5. пишем в БД
        app_con <- dbConnect(SQLite(), conf$database_settings$app_data_base)
        
        DBI::dbExecute(
          app_con,
          "
      INSERT INTO auth_sessions (user_login, token_hash, expires_at)
      VALUES (?, ?, datetime('now', '+30 days'))
      ",
          params = list(user_login, token_hash)
        )
        
        dbDisconnect(app_con)
        
        # 6. кладём cookie в браузер
        session$sendCustomMessage("setAuthCookie", raw_token)
        
      } else {
        output$login_message <- renderText("Неверный логин или пароль")
      }
    })
    
    
    return(list(user = user))
    
  })
}
