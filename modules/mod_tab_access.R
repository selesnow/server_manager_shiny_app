mod_access_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Скрипт для отслеживания двойного клика по строке таблицы пользователей
    tags$script(HTML(paste0("
    $(document).ready(function() {
      $(document).on('dblclick', '#", id, "-users_table tbody tr', function(e) {
        e.preventDefault();
        
        var username = $(this).attr('data-username');
        
        if (!username) {
          username = $(this).find('td:eq(0)').text().trim(); // fallback на первую ячейку (логин при rownames = FALSE)
        }
        
        console.log('Double-click detected on row! Username resolved to:', username);
        
        // Отправляем событие в Shiny
        Shiny.setInputValue('", id, "-users_table_cell_clicked', {
          username: username,
          value: 'dblclick',
          timestamp: new Date().getTime()
        }, {priority: 'event'});
      });
    });
    "))),
    h3("Управление доступом"),
    h4("Список пользователей"),
    DTOutput(ns("users_table")),
    hr(),
    h4("Управление пользователями"),
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
    ),
    hr(),
    
    # --- Блок Access management ---
    h4("Управление доступом к функционалу приложения для ролей"),
    uiOutput(ns("role_editor")),
    div(
      style = "text-align: right; margin-top: 10px;",
      actionButton(ns("save_roles"), "💾 Сохранить роли", class = "btn-success")
    ),
    hr(),
    
    # --- Логирование и базы на одной строке ---
    h4("Прочие настройки"),
    fluidRow(
      column(
        2,
        h5("⚡ Логирование"),
        uiOutput(ns("logging_editor")),
        div(
          style = "text-align: right; margin-top: 10px;",
          actionButton(ns("save_logging"), "💾 Сохранить логирование", class = "btn-success")
        )
      ),
      column(
        2,
        h5("💾 Хранение данных"),
        uiOutput(ns("database_editor")),
        div(
          style = "text-align: right; margin-top: 10px;",
          actionButton(ns("save_database"), "💾 Сохранить базы", class = "btn-success")
        )
      )
    )
  )
}

mod_access_server <- function(id, conn, auth, session_id, conf_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # === Работа с пользователями ===
    users_trigger <- reactiveVal(0)
    
    load_users <- reactive({
      users_trigger()
      dbGetQuery(conn, "SELECT login, role FROM users")
    })
    
    output$users_table <- renderDT({
      df <- load_users()
      datatable(df,
                rownames = FALSE, # Отключаем нумерацию строк для сопоставления data[0] с login
                options = list(
                  pageLength = 5,
                  rowCallback = DT::JS(
                    "function(row, data, index) {",
                    "  $(row).attr('data-username', data[0]);", # data[0] - логин пользователя
                    "}"
                  )
                ),
                selection = 'none') %>%
        formatStyle(columns = 1:ncol(df), cursor = 'pointer')
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
      write_action_log(user = auth$user()$login, func = 'User add', session_id, value = input$new_login)
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
      write_action_log(user = auth$user()$login, func = 'User remove', session_id, value = input$user_to_delete)
      dbExecute(conn, "DELETE FROM users WHERE login = ?", params = list(input$user_to_delete))
      showNotification("Пользователь удалён", type = "message")
      users_trigger(users_trigger() + 1)
    })
    
    observeEvent(input$change_role, {
      write_action_log(user = auth$user()$login, func = 'User change role', session_id, value = input$user_to_change)
      dbExecute(conn,
                "UPDATE users SET role = ? WHERE login = ?",
                params = list(input$updated_role, input$user_to_change))
      showNotification("Роль обновлена", type = "message")
      users_trigger(users_trigger() + 1)
    })
    
    observeEvent(input$reset_password, {
      write_action_log(user = auth$user()$login, func = 'User password reset', session_id, value = input$user_to_reset)
      if (input$new_user_password != input$confirm_user_password) {
        showNotification("Пароли не совпадают!", type = "error")
        return()
      }
      
      dbExecute(conn,
                "UPDATE users SET password = ? WHERE login = ?",
                params = list(input$new_user_password, input$user_to_reset))
      showNotification("Пароль обновлён", type = "message")
    })
    
    
    # === Работа с YAML-конфигом ===
    conf <- conf_rv
    
    # --- UI для ролей ---
    output$role_editor <- renderUI({
      conf_list <- conf()
      tab_nodes <- conf_list$access_managemet
      role_choices <- c("admin", "user", "viewer")
      cols_per_row <- 4
      nodes <- names(tab_nodes)
      rows <- split(nodes, ceiling(seq_along(nodes) / cols_per_row))
      
      tagList(
        lapply(rows, function(row_nodes) {
          fluidRow(
            lapply(row_nodes, function(node) {
              column(
                width = floor(12 / cols_per_row),
                selectInput(
                  ns(paste0("roles_", node)),
                  label = node,
                  choices = role_choices,
                  selected = tab_nodes[[node]],
                  multiple = TRUE
                )
              )
            })
          )
        })
      )
    })
    
    # --- UI для логирования ---
    output$logging_editor <- renderUI({
      conf_list <- conf()
      log_nodes <- conf_list$logging
      tagList(
        lapply(names(log_nodes), function(log_type) {
          checkboxInput(
            ns(paste0("log_", log_type)),
            label = log_type,
            value = isTRUE(log_nodes[[log_type]])
          )
        })
      )
    })
    
    # --- UI для баз данных ---
    output$database_editor <- renderUI({
      conf_list <- conf()
      db_nodes <- conf_list$database_settings
      tagList(
        lapply(names(db_nodes), function(db_name) {
          textInput(
            ns(paste0("db_", db_name)),
            label = db_name,
            value = db_nodes[[db_name]]
          )
        })
      )
    })
    
    
    # --- Save роли ---
    observeEvent(input$save_roles, {
      write_action_log(user = auth$user()$login, func = 'Access config change', session_id)
      new_conf <- conf()
      tab_nodes <- names(new_conf$access_managemet)
      for (node in tab_nodes) {
        input_id <- paste0("roles_", node)
        if (!is.null(input[[input_id]])) {
          new_conf$access_managemet[[node]] <- input[[input_id]]
        }
      }
      conf(new_conf)
      yaml::write_yaml(new_conf, "config.yaml")
      showNotification("✅ Конфигурация ролей обновлена", type = "message")
    })
    
    # --- Save логирование ---
    observeEvent(input$save_logging, {
      write_action_log(user = auth$user()$login, func = 'Logging config change', session_id)
      new_conf <- conf()
      log_nodes <- names(new_conf$logging)
      for (log_type in log_nodes) {
        input_id <- paste0("log_", log_type)
        if (!is.null(input[[input_id]])) {
          new_conf$logging[[log_type]] <- input[[input_id]]
        }
      }
      conf(new_conf)
      yaml::write_yaml(new_conf, "config.yaml")
      showNotification("✅ Настройки логирования обновлены", type = "message")
    })
    
    # --- Save базы ---
    observeEvent(input$save_database, {
      write_action_log(user = auth$user()$login, func = 'Database config change', session_id)
      new_conf <- conf()
      db_nodes <- names(new_conf$database_settings)
      for (db_name in db_nodes) {
        input_id <- paste0("db_", db_name)
        if (!is.null(input[[input_id]])) {
          new_conf$database_settings[[db_name]] <- input[[input_id]]
        }
      }
      conf(new_conf)
      yaml::write_yaml(new_conf, "config.yaml")
      showNotification("✅ Пути к базам обновлены", type = "message")
    })
    
    # === Карточка пользователя и управление им (двойной клик) ===
    current_modal_username <- reactiveVal(NULL)
    
    # Окно с карточкой пользователя при двойном клике по строке таблицы пользователей
    observeEvent(input$users_table_cell_clicked, {
      click_info <- input$users_table_cell_clicked
      req(click_info)
      
      # Проверяем, что это двойной клик
      if (!is.null(click_info$value) && click_info$value == "dblclick") {
        username <- click_info$username
        req(username)
        
        # Получаем данные пользователя из БД
        user_info <- dbGetQuery(conn, "SELECT login, role FROM users WHERE login = ?", params = list(username))
        req(nrow(user_info) > 0)
        
        user_role_val <- user_info$role[1]
        
        # Запрос даты регистрации из логов
        reg_info <- dbGetQuery(conn, "SELECT datetime FROM action_log WHERE func_name = 'User add' AND value = ? ORDER BY datetime ASC LIMIT 1", params = list(username))
        reg_date <- if (nrow(reg_info) > 0) reg_info$datetime[1] else "Неизвестно (создан до логирования)"
        
        # Запрос последней активности из логов
        last_act_info <- dbGetQuery(conn, "
          SELECT MAX(dt) as last_act FROM (
            SELECT MAX(datetime) as dt FROM action_log WHERE user = ?
            UNION
            SELECT MAX(datetime) as dt FROM session_log WHERE user = ?
          )
        ", params = list(username, username))
        
        last_activity <- if (nrow(last_act_info) > 0 && !is.na(last_act_info$last_act[1])) last_act_info$last_act[1] else "Нет активности в логах"
        
        # Показываем модальное окно
        showModal(modalDialog(
          title = paste("Карточка пользователя:", username),
          size = "m",
          tags$head(
            tags$style(HTML("
              .modal-content {
                  background-color: var(--bg-card) !important;
                  color: var(--text-primary) !important;
                  border: 1px solid var(--border-color) !important;
                  border-radius: 12px !important;
              }
              .modal-header, .modal-footer {
                  border: none !important;
              }
              .modal-title {
                  color: var(--text-primary) !important;
                  font-weight: bold;
              }
              .modal-body strong {
                  color: var(--primary) !important;
              }
            "))
          ),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Закрыть")
          ),
          
          # Карточка с информацией
          div(
            div(class = "mb-2", strong("Логин: "), span(username)),
            div(class = "mb-2", strong("Текущая роль: "), span(user_role_val)),
            div(class = "mb-2", strong("Дата регистрации: "), span(reg_date)),
            div(class = "mb-2", strong("Последняя активность: "), span(last_activity)),
            
            tags$hr(),
            
            # Раздел управления пользователем внутри модалки
            h4("Управление пользователем", class = "mb-3", style = "color: var(--primary); font-weight: 600;"),
            
            # 1. Изменение роли
            div(
              class = "mb-4 p-3", style = "background-color: rgba(99, 102, 241, 0.03); border: 1px solid var(--border-color); border-radius: 8px;",
              h5("🛠 Изменить роль", style = "font-weight: 600; margin-top: 0; margin-bottom: 10px;"),
              fluidRow(
                column(8, selectInput(ns("modal_updated_role"), "Новая роль:", choices = c("admin", "user", "viewer"), selected = user_role_val)),
                column(4, actionButton(ns("modal_change_role"), "Изменить роль", class = "btn btn-warning w-100", style = "margin-top: 25px;"))
              )
            ),
            
            # 2. Сброс пароля
            div(
              class = "mb-4 p-3", style = "background-color: rgba(99, 102, 241, 0.03); border: 1px solid var(--border-color); border-radius: 8px;",
              h5("🔐 Сброс пароля", style = "font-weight: 600; margin-top: 0; margin-bottom: 10px;"),
              fluidRow(
                column(4, passwordInput(ns("modal_new_password"), "Новый пароль")),
                column(4, passwordInput(ns("modal_confirm_password"), "Подтвердите")),
                column(4, actionButton(ns("modal_reset_password"), "Сбросить пароль", class = "btn btn-warning w-100", style = "margin-top: 25px;"))
              )
            ),
            
            # 3. Удаление пользователя
            if (username != "root" && username != auth$user()$login) {
              div(
                class = "mb-3 p-3", style = "background-color: rgba(239, 68, 68, 0.03); border: 1px solid rgba(239, 68, 68, 0.2); border-radius: 8px;",
                h5("🗑️ Удалить пользователя", style = "font-weight: 600; color: var(--danger); margin-top: 0; margin-bottom: 10px;"),
                p("Внимание: это действие необратимо!", style = "font-size: 0.85em; color: var(--text-secondary); margin-bottom: 10px;"),
                actionButton(ns("modal_delete_user"), "Удалить аккаунт", class = "btn btn-danger")
              )
            } else {
              div(
                class = "mb-3 p-3", style = "background-color: rgba(148, 163, 184, 0.05); border: 1px solid var(--border-color); border-radius: 8px;",
                p("Удаление этого пользователя ограничено (системный аккаунт или текущая сессия).", style = "font-size: 0.85em; color: var(--text-secondary); margin-bottom: 0;")
              )
            }
          )
        ))
        
        current_modal_username(username)
      }
    }, ignoreInit = TRUE)
    
    # --- Изменение роли из модалки ---
    observeEvent(input$modal_change_role, {
      req(current_modal_username())
      username <- current_modal_username()
      
      write_action_log(user = auth$user()$login, func = 'User change role', session_id, value = username)
      dbExecute(conn,
                "UPDATE users SET role = ? WHERE login = ?",
                params = list(input$modal_updated_role, username))
      
      showNotification(paste("Роль пользователя", username, "обновлена"), type = "message")
      users_trigger(users_trigger() + 1)
      removeModal()
    })
    
    # --- Сброс пароля из модалки ---
    observeEvent(input$modal_reset_password, {
      req(current_modal_username())
      username <- current_modal_username()
      
      if (input$modal_new_password != input$modal_confirm_password) {
        showNotification("Пароли не совпадают!", type = "error")
        return()
      }
      
      write_action_log(user = auth$user()$login, func = 'User password reset', session_id, value = username)
      dbExecute(conn,
                "UPDATE users SET password = ? WHERE login = ?",
                params = list(input$modal_new_password, username))
      
      showNotification(paste("Пароль пользователя", username, "обновлён"), type = "message")
      removeModal()
    })
    
    # --- Удаление из модалки ---
    observeEvent(input$modal_delete_user, {
      req(current_modal_username())
      username <- current_modal_username()
      
      if (username == "root" || username == auth$user()$login) {
        showNotification("Удаление этого пользователя запрещено!", type = "error")
        return()
      }
      
      write_action_log(user = auth$user()$login, func = 'User remove', session_id, value = username)
      dbExecute(conn, "DELETE FROM users WHERE login = ?", params = list(username))
      
      showNotification(paste("Пользователь", username, "удалён"), type = "message")
      users_trigger(users_trigger() + 1)
      removeModal()
    })
  })
}

