# modules/mod_auth.R

mod_auth_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("auth_ui"))  # динамически отрисуем твой login form
}

mod_auth_server <- function(id, logged_in, user_role, check_user_fun, conn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # подхватываем логин пользователя
    user <- reactiveVal(NULL)

    # есть ли в базе хоть один пользователь, если нет, вместо входа показываем
    # форму создания первого пользователя (администратора)
    users_exist <- reactiveVal(
      DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM users")$n > 0
    )

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
            #auth-login_message, #auth-setup_message {
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
                       if (users_exist()) {
                         tagList(
                           tags$h2(id = "title-panel", "Авторизация"),
                           textInput(ns("login"), "Логин"),
                           passwordInput(ns("password"), "Пароль"),
                           actionButton(ns("login_btn"), "Войти"),
                           textOutput(ns("login_message"))
                         )
                       } else {
                         tagList(
                           tags$h2(id = "title-panel", "Создание администратора"),
                           tags$p(
                             style = "color: #94a3b8; font-size: 0.85em; text-align: center;",
                             "В приложении ещё нет пользователей. Первый созданный пользователь получает роль admin."
                           ),
                           textInput(ns("setup_login"), "Логин"),
                           passwordInput(ns("setup_password"), "Пароль"),
                           passwordInput(ns("setup_password_confirm"), "Подтвердите пароль"),
                           actionButton(ns("setup_btn"), "Создать администратора"),
                           textOutput(ns("setup_message"))
                         )
                       }
                )
              )
          )
        )
      }
    })
    
    # завершение входа: фиксируем пользователя, поднимаем флаги авторизации,
    # создаём persistent token и кладём его в cookie
    finish_login <- function(user_login, role) {

      # 1. сохраняем пользователя в reactiveVal
      user(list(
        login = user_login,
        role  = role
      ))

      # 2. поднимаем флаги авторизации
      logged_in(TRUE)
      user_role(role)

      # 3. генерим persistent token
      raw_token  <- openssl::rand_bytes(32) |> openssl::base64_encode()
      token_hash <- openssl::sha256(raw_token)

      # 4. пишем в БД
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

      # 5. кладём cookie в браузер
      session$sendCustomMessage("setAuthCookie", raw_token)
    }

    # Защита от подбора пароля (R/login_protection.R). Если проверка блокировок
    # недоступна (например, нет таблицы), вход продолжает работать по паролю,
    # а ошибка пишется в лог приложения.
    lock_seconds <- function(login) {
      tryCatch(login_lock_remaining(conn, login), error = function(e) {
        message("Проверка блокировки входа недоступна: ", conditionMessage(e))
        0L
      })
    }

    lock_text <- function(seconds) {
      paste0("Слишком много неудачных попыток входа. Повторите через ", max(1, ceiling(seconds / 60)), " мин.")
    }

    observeEvent(input$login_btn, {

      login <- input$login

      # общий лимит неудачных попыток по всем логинам за минуту
      throttled <- tryCatch(login_global_throttled(conn), error = function(e) FALSE)
      if (isTRUE(throttled)) {
        output$login_message <- renderText("Слишком много попыток входа. Повторите через минуту.")
        return()
      }

      # логин заблокирован: пароль не проверяем и попытку не записываем,
      # что бы блокировка не продлевалась
      remaining <- lock_seconds(login)
      if (remaining > 0) {
        output$login_message <- renderText(lock_text(remaining))
        return()
      }

      res <- check_user_fun(login, input$password)

      if (!is.null(res) && isTRUE(res$blocked[1] == 1)) {
        # пароль верный, но учётная запись заблокирована администратором: сообщаем
        # об этом только тому, кто знает пароль, что бы не раскрывать список логинов
        try(write_action_log(user = normalize_login(login), func = 'Blocked login attempt',
                             session_id = session$token, value = normalize_login(login)), silent = TRUE)
        output$login_message <- renderText("Учётная запись заблокирована администратором. Обратитесь к администратору.")
        return()
      }

      if (!is.null(res)) {
        try(record_login_attempt(conn, login, success = TRUE), silent = TRUE)
        # логин берём из базы, а не из введённой строки
        finish_login(res$login[1], res$role[1])
      } else {
        try(record_login_attempt(conn, login, success = FALSE), silent = TRUE)

        # эта попытка могла привести к блокировке
        remaining <- lock_seconds(login)
        if (remaining > 0) {
          try(write_action_log(user = normalize_login(login), func = 'Login locked',
                               session_id = session$token, value = normalize_login(login)), silent = TRUE)
          output$login_message <- renderText(lock_text(remaining))
        } else {
          try(write_action_log(user = normalize_login(login), func = 'Login failed',
                               session_id = session$token, value = normalize_login(login)), silent = TRUE)
          output$login_message <- renderText("Неверный логин или пароль")
        }
      }
    })

    # создание первого пользователя, срабатывает только если пользователей ещё нет
    observeEvent(input$setup_btn, {

      # на случай если пользователь уже создан в другой вкладке или сессии
      if (DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM users")$n > 0) {
        users_exist(TRUE)
        return()
      }

      login    <- trimws(input$setup_login)
      password <- input$setup_password

      error <- validate_new_login(login) %||% validate_new_password(password)
      if (is.null(error) && !identical(password, input$setup_password_confirm)) {
        error <- "Пароли не совпадают"
      }

      if (!is.null(error)) {
        output$setup_message <- renderText(error)
        return()
      }

      # запрос создаёт пользователя только если таблица users пустая
      if (create_first_admin(conn, login, password)) {
        write_action_log(user = login, func = 'User add', session_id = session$token, value = login)
        users_exist(TRUE)
        finish_login(login, "admin")
      } else {
        users_exist(TRUE)
      }
    })
    
    
    return(list(user = user))
    
  })
}
