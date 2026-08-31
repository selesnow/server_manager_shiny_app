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
              background: radial-gradient(circle at center, #1e1b4b 0%, #0f172a 100%);
              font-family: 'Plus Jakarta Sans', sans-serif;
            }
            .login-container .form-container {
              width: 100%;
              max-width: 400px;
              padding: 35px 40px;
              border: 1px solid rgba(255, 255, 255, 0.08);
              border-radius: 16px;
              background-color: rgba(30, 41, 59, 0.7);
              backdrop-filter: blur(16px);
              box-shadow: 0 20px 25px -5px rgba(0, 0, 0, 0.5), 0 10px 10px -5px rgba(0, 0, 0, 0.5);
            }
            .login-container .form-container .form-group {
              margin-bottom: 20px;
            }
            .login-container .form-container label {
              color: #94a3b8;
              font-weight: 500;
              font-size: 0.85em;
              margin-bottom: 6px;
            }
            .login-container .form-container input {
              width: 100%;
              padding: 12px 16px;
              background-color: rgba(15, 23, 42, 0.6) !important;
              color: #f8fafc !important;
              border: 1px solid rgba(255, 255, 255, 0.1) !important;
              border-radius: 8px !important;
              transition: all 0.2s ease;
            }
            .login-container .form-container input:focus {
              border-color: #6366f1 !important;
              box-shadow: 0 0 0 3px rgba(99, 102, 241, 0.25) !important;
              outline: none;
            }
            .login-container .form-container button {
              width: 100%;
              padding: 12px;
              background-color: #6366f1 !important;
              color: #ffffff !important;
              border: none !important;
              border-radius: 8px !important;
              font-weight: 600 !important;
              margin-top: 15px;
              transition: all 0.2s ease !important;
              box-shadow: 0 4px 6px -1px rgba(99, 102, 241, 0.2);
            }
            .login-container .form-container button:hover {
              background-color: #4f46e5 !important;
              transform: translateY(-1px);
              box-shadow: 0 10px 15px -3px rgba(99, 102, 241, 0.3);
            }
            .login-container .form-container button:active {
              transform: translateY(0);
            }
            #title-panel {
              text-align: center;
              color: #f8fafc;
              font-size: 26px;
              font-weight: 700;
              letter-spacing: -0.5px;
              margin-bottom: 25px;
              margin-top: 0;
            }
            #auth-login_message {
              text-align: center;
              color: #ef4444;
              font-size: 0.9em;
              margin-top: 15px;
              font-weight: 500;
              display: block;
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
