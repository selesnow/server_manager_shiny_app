mod_access_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Управление доступом"),
    h4("Список пользователей"),
    DTOutput(ns("users_table")),
    hr(),
    
    fluidRow(
      column(3,
             h5("➕ Добавить"),
             textInput(ns("new_login"), "Логин"),
             passwordInput(ns("new_password"), "Пароль"),
             passwordInput(ns("confirm_password"), "Подтвердите"),
             selectInput(ns("new_role"), "Роль", choices = c("admin", "user", 'viewer')),
             actionButton(ns("add_user"), "Добавить", class = "btn btn-success")
      ),
      column(3,
             h5("🗑️ Удалить"),
             uiOutput(ns("user_selector_delete")),
             actionButton(ns("delete_user"), "Удалить", class = "btn btn-danger")
      ),
      column(3,
             h5("🛠 Изменить роль"),
             uiOutput(ns("user_selector_role")),
             selectInput(ns("updated_role"), "Новая роль", choices = c("admin", "user", 'viewer')),
             actionButton(ns("change_role"), "Изменить", class = "btn btn-warning")
      ),
      column(3,
             h5("🔐 Сброс пароля"),
             uiOutput(ns("user_selector_password")),
             passwordInput(ns("new_user_password"), "Новый пароль"),
             passwordInput(ns("confirm_user_password"), "Подтвердите"),
             actionButton(ns("reset_password"), "Сбросить", class = "btn btn-warning")
      )
    )
  )
}

mod_access_server <- function(id, conn, auth, session_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # триггер для обновления users
    users_trigger <- reactiveVal(0)
    
    load_users <- reactive({
      users_trigger()
      dbGetQuery(conn, "SELECT login, role FROM users")
    })
    
    output$users_table <- renderDT({
      datatable(load_users(), options = list(pageLength = 5))
    })
    
    update_user_inputs <- function() {
      updateSelectInput(inputId = ns("user_to_delete"), choices = load_users()$login)
      updateSelectInput(inputId = ns("user_to_change"), choices = load_users()$login)
      updateSelectInput(inputId = ns("user_to_reset"), choices = load_users()$login)
    }
    
    observe({
      users_trigger()
      update_user_inputs()
    })
    
    output$user_selector_delete <- renderUI({
      selectInput(ns("user_to_delete"), "Пользователь", choices = load_users()$login)
    })
    output$user_selector_role <- renderUI({
      selectInput(ns("user_to_change"), "Пользователь", choices = load_users()$login)
    })
    output$user_selector_password <- renderUI({
      selectInput(ns("user_to_reset"), "Пользователь", choices = load_users()$login)
    })
    
    observeEvent(input$add_user, {
      write_action_log(user = auth$user()$login, func = 'User add', session_id)
      if (input$new_password != input$confirm_password) {
        showNotification("Пароли не совпадают!", type = "error")
        return()
      }
      
      existing <- dbGetQuery(conn, "SELECT * FROM users WHERE login = ?", params = list(input$new_login))
      if (nrow(existing) > 0) {
        showNotification("Пользователь уже существует!", type = "error")
        return()
      }
      
      dbExecute(conn,
                "INSERT INTO users (login, password, role) VALUES (?, ?, ?)",
                params = list(input$new_login, input$new_password, input$new_role))
      
      showNotification("Пользователь добавлен!", type = "message")
      users_trigger(users_trigger() + 1)
    })
    
    observeEvent(input$delete_user, {
      write_action_log(user = auth$user()$login, func = 'User remove', session_id)
      dbExecute(conn, "DELETE FROM users WHERE login = ?", params = list(input$user_to_delete))
      showNotification("Пользователь удалён", type = "message")
      users_trigger(users_trigger() + 1)
    })
    
    observeEvent(input$change_role, {
      write_action_log(user = auth$user()$login, func = 'User change role', session_id)
      dbExecute(conn,
                "UPDATE users SET role = ? WHERE login = ?",
                params = list(input$updated_role, input$user_to_change))
      showNotification("Роль обновлена", type = "message")
      users_trigger(users_trigger() + 1)
    })
    
    observeEvent(input$reset_password, {
      write_action_log(user = auth$user()$login, func = 'User password reset', session_id)
      if (input$new_user_password != input$confirm_user_password) {
        showNotification("Пароли не совпадают!", type = "error")
        return()
      }
      
      dbExecute(conn,
                "UPDATE users SET password = ? WHERE login = ?",
                params = list(input$new_user_password, input$user_to_reset))
      showNotification("Пароль обновлён", type = "message")
    })
  })
}
