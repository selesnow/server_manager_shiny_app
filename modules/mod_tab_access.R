mod_access_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Управление доступом"),

    # Пользователи: таблица, карточка по двойному клику, добавление, роли, пароли,
    # блокировки, удаление (модуль пакета appauth)
    aa_users_ui(ns("users")),
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

#' Дополнительный блок карточки пользователя: данные из логов приложения
#'
#' Пакет appauth логов приложения не читает, поэтому дату регистрации и последнюю
#' активность карточке даёт слот приложения.
user_card_log_info <- function(conn, login, ns) {
  reg_info <- dbGetQuery(conn, "SELECT datetime FROM action_log WHERE func_name = 'User add' AND value = ? ORDER BY datetime ASC LIMIT 1", params = list(login))
  reg_date <- if (nrow(reg_info) > 0) reg_info$datetime[1] else "Неизвестно (создан до логирования)"

  last_act_info <- dbGetQuery(conn, "
    SELECT MAX(dt) as last_act FROM (
      SELECT MAX(datetime) as dt FROM action_log WHERE user = ?
      UNION
      SELECT MAX(datetime) as dt FROM session_log WHERE user = ?
    )
  ", params = list(login, login))
  last_activity <- if (nrow(last_act_info) > 0 && !is.na(last_act_info$last_act[1])) last_act_info$last_act[1] else "Нет активности в логах"

  div(
    class = "aa-card-section",
    div(strong("Дата регистрации: "), span(reg_date)),
    div(strong("Последняя активность: "), span(last_activity))
  )
}

mod_access_server <- function(id, conn, auth, session_id, conf_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Серверная проверка: все действия этой вкладки доступны только админу.
    # Роль админа проверяется отдельно от конфига, что бы нельзя было выдать
    # управление пользователями другой роли и повысить себе права.
    require_access_admin <- function() {
      require_permission(auth, conf_rv, "Доступ", session_id)
      if (!identical(auth$user()$role, auth_settings$admin_role)) {
        showNotification("Недостаточно прав для этого действия", type = "error")
        req(FALSE)
      }
    }

    # === Работа с пользователями (модуль пакета appauth) ===
    # Добавление, смена роли, сброс пароля, блокировка, снятие блокировки входа и
    # удаление проверяются на сервере пакетом (в том числе правила «нельзя
    # заблокировать или удалить себя и последнего администратора»).
    users <- aa_users_server(
      "users", conn, auth, auth_settings,
      on_event      = log_auth_event,
      extra_card_ui = function(login, ns) user_card_log_info(conn, login, ns)
    )

    # статусы блокировок со временем меняются: перерисовываем таблицу раз в минуту
    observe({
      invalidateLater(60000, session)
      users$refresh()
    })

    # === Работа с YAML-конфигом ===
    conf <- conf_rv
    
    # --- UI для ролей ---
    output$role_editor <- renderUI({
      conf_list <- conf()
      tab_nodes <- conf_list$access_managemet
      role_choices <- auth_settings$roles
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
      require_access_admin()
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
      require_access_admin()
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
      require_access_admin()
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

  })
}
