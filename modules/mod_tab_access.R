# Вкладка «Доступ»: пользователи (модуль пакета appauth) и настройки приложения
# (права ролей, безопасность входа, журналы, базы данных, резервные копии конфига).
# Описание настроек, проверки и подписи лежат в R/access_settings.R, копии конфига в
# R/config_backups.R, стили в www/css/main-styles.css (классы cfg-*).
#
# Когда что применяется (приложение читает config.yaml один раз при запуске в общую переменную
# conf, у каждой сессии своя копия conf_rv):
#  * журналы: сразу для всех (функции записи читают общую conf);
#  * права ролей и настройки входа: общая conf обновляется сразу, поэтому они действуют для
#    новых сессий, уже открытые сессии подхватывают их после обновления страницы;
#  * пути к базам: только после перезапуска приложения. В работающем приложении они не
#    меняются: сессии уже держат подключение к старой базе, а несуществующий файл
#    при первом обращении превратился бы в пустую базу без таблиц.

#' Строка-переключатель с подсказкой
#'
#' @param input_id идентификатор уже внутри пространства имён модуля
#' @param label название
#' @param hint подсказка под названием
#' @param value текущее значение
cfg_switch <- function(input_id, label, hint = "", value = FALSE) {
  div(
    class = "cfg-field cfg-switch-row",
    div(class = "cfg-switch", checkboxInput(input_id, label, value = isTRUE(value))),
    if (nzchar(hint)) div(class = "cfg-hint", hint)
  )
}

#' Нижняя панель раздела настроек: пометка об изменениях и кнопки
cfg_actions <- function(dirty_output, ..., ns_save, label = "Сохранить") {
  div(
    class = "cfg-actions",
    div(class = "cfg-actions-note", dirty_output),
    div(class = "cfg-actions-buttons", ..., actionButton(ns_save, paste0("💾 ", label), class = "btn-success"))
  )
}

mod_access_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Управление доступом"),

    # Пользователи: таблица, карточка по двойному клику, добавление, роли, пароли,
    # блокировки, удаление (модуль пакета appauth)
    aa_users_ui(ns("users")),
    hr(),

    h4("Настройки приложения"),
    div(
      class = "cfg-lead",
      "Журналы применяются сразу. Права ролей и настройки входа действуют для новых сессий ",
      "(открытым сессиям нужно обновить страницу). Пути к базам применяются после перезапуска приложения. ",
      "Перед каждым изменением предыдущий конфиг сохраняется на вкладке «Резервные копии»."
    ),

    div(class = "cfg-tabs", tabsetPanel(
      id = ns("settings_tabs"),
      type = "tabs",

      tabPanel(
        "Права ролей",
        div(
          class = "cfg-section",
          div(class = "cfg-section-title", "Кому что доступно"),
          div(class = "cfg-section-text", "Отметьте роли, у которых есть право. Право на вкладку «Доступ» у администратора отключить нельзя."),
          uiOutput(ns("role_editor")),
          cfg_actions(uiOutput(ns("roles_dirty"), inline = TRUE), ns_save = ns("save_roles"), label = "Сохранить права")
        )
      ),

      tabPanel(
        "Безопасность входа",
        div(
          class = "cfg-section",
          div(class = "cfg-section-title", "Защита входа и пароли"),
          div(class = "cfg-section-text", "Защита от подбора пароля, правила для новых паролей и сроки автовхода."),
          uiOutput(ns("security_editor")),
          cfg_actions(
            uiOutput(ns("security_dirty"), inline = TRUE),
            actionButton(ns("reset_security"), "↺ По умолчанию", class = "btn-reset"),
            ns_save = ns("save_security"), label = "Сохранить настройки входа"
          )
        )
      ),

      tabPanel(
        "Журналы",
        div(
          class = "cfg-section",
          div(class = "cfg-section-title", "Что записывать в журналы"),
          div(class = "cfg-section-text", "Выключенный журнал перестаёт пополняться сразу, уже записанное не удаляется."),
          uiOutput(ns("logging_editor")),
          cfg_actions(
            uiOutput(ns("logging_dirty"), inline = TRUE),
            actionButton(ns("reset_logging"), "↺ По умолчанию", class = "btn-reset"),
            ns_save = ns("save_logging"), label = "Сохранить журналы"
          )
        )
      ),

      tabPanel(
        "Базы данных",
        div(
          class = "cfg-section cfg-danger",
          div(class = "cfg-section-title", "Пути к базам данных"),
          div(
            class = "cfg-section-text",
            "Выберите базу из списка или введите путь вручную. Изменение вступает в силу после перезапуска приложения, ",
            "сейчас оно продолжает работать со старыми базами. Если указать ещё не созданный файл базы приложения, ",
            "при запуске он будет создан с нуля, а старая база останется нетронутой (в новой базе нужно будет создать первого администратора)."
          ),
          uiOutput(ns("database_editor")),
          cfg_actions(uiOutput(ns("database_dirty"), inline = TRUE), ns_save = ns("save_database"), label = "Сохранить пути")
        ),
        div(
          class = "cfg-section",
          div(class = "cfg-section-title", "Создать новую пустую базу"),
          div(
            class = "cfg-section-text",
            "Создаёт файл базы с нужными таблицами прямо сейчас. Приложение продолжит работать со старой базой, ",
            "пока вы не выберете новую выше, не сохраните пути и не перезапустите приложение. Имя без папки создаётся в папке приложения."
          ),
          div(
            class = "cfg-inline",
            textInput(ns("newdb_path"), NULL, value = paste0("app_", format(Sys.Date(), "%Y%m%d"), ".db"), placeholder = "app_новая.db", width = "420px"),
            actionButton(ns("create_db_now"), "➕ Создать базу", class = "btn-success")
          )
        )
      ),

      tabPanel(
        "Резервные копии",
        div(
          class = "cfg-section",
          div(class = "cfg-section-title", "Резервные копии конфига"),
          div(
            class = "cfg-section-text",
            "Перед каждым изменением настроек предыдущий config.yaml сохраняется (хранятся последние ", CONFIG_BACKUP_KEEP, " копий). ",
            "При восстановлении права ролей, настройки входа и журналы возвращаются из копии, а пути к базам остаются текущими. ",
            "Текущий конфиг перед восстановлением тоже сохраняется."
          ),
          uiOutput(ns("backups_editor")),
          div(
            class = "cfg-actions",
            div(class = "cfg-actions-note"),
            div(class = "cfg-actions-buttons", actionButton(ns("restore_backup"), "↩ Восстановить выбранную копию", class = "btn-warning"))
          )
        )
      )
    ))
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
    # удаление проверяются на сервере пакета (в том числе правила «нельзя
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

    # === Настройки приложения (config.yaml) ===
    conf <- conf_rv

    or_default <- function(x, default) if (is.null(x)) default else x

    dirty_badge <- function(is_dirty) {
      if (isTRUE(is_dirty)) span(class = "cfg-dirty", "● Есть несохранённые изменения")
    }

    running_conf <- function() get0("conf", envir = globalenv(), ifnotfound = list())

    read_file_conf <- function() tryCatch(yaml::read_yaml("config.yaml"), error = function(e) conf())

    backups_version <- reactiveVal(0)
    db_version <- reactiveVal(0)

    # применяет раздел к работающему приложению: общая conf (её читают функции журналов,
    # новые сессии и проверки прав) и conf_rv этой сессии
    apply_running <- function(section, value) {
      running <- running_conf()
      running[[section]] <- value
      assign("conf", running, envir = globalenv())

      current <- conf()
      current[[section]] <- value
      conf(current)
    }

    # записывает раздел в config.yaml, предыдущий файл сохраняется в config_backups.
    # Раздел берётся в файле, а не из копии сессии: другой администратор мог изменить соседний раздел.
    save_config_section <- function(section, value, apply = TRUE) {
      file_conf <- read_file_conf()
      backup_config()
      file_conf[[section]] <- value
      yaml::write_yaml(file_conf, "config.yaml")

      if (isTRUE(apply)) apply_running(section, value)
      backups_version(backups_version() + 1)
      invisible(file_conf)
    }

    # --- Права ролей: матрица «право - роль» ------------------------------------------
    matrix_state <- function() {
      cfg <- conf()$access_managemet
      perms <- names(cfg)
      roles <- auth_settings$roles

      stats::setNames(lapply(seq_along(perms), function(i) {
        checked <- vapply(seq_along(roles), function(j) isTRUE(input[[matrix_input_id(i, j)]]), logical(1))
        roles_from_matrix(checked, roles, cfg[[perms[i]]], perms[i], auth_settings$admin_role)
      }), perms)
    }

    output$role_editor <- renderUI({
      cfg <- conf()$access_managemet
      perms <- names(cfg)
      roles <- auth_settings$roles
      layout <- permission_layout(perms)

      header <- tags$tr(
        tags$th("Право"),
        lapply(roles, function(r) tags$th(class = "cfg-role-col", r))
      )

      body <- lapply(names(layout), function(group) {
        tagList(
          tags$tr(class = "cfg-group", tags$td(colspan = length(roles) + 1, group)),
          lapply(layout[[group]], function(perm) {
            i <- match(perm, perms)
            hint <- unname(PERMISSION_HINTS[perm])
            if (is.na(hint)) hint <- ""

            tags$tr(
              tags$td(div(class = "cfg-perm-name", perm), if (nzchar(hint)) div(class = "cfg-hint", hint)),
              lapply(seq_along(roles), function(j) {
                locked <- identical(perm, "Доступ") && identical(roles[j], auth_settings$admin_role)
                cell <- checkboxInput(ns(matrix_input_id(i, j)), label = NULL, value = locked || roles[j] %in% cfg[[perm]])
                tags$td(class = "cfg-cell", if (locked) shinyjs::disabled(cell) else cell)
              })
            )
          })
        )
      })

      div(class = "cfg-matrix-wrap", tags$table(class = "cfg-matrix", tags$thead(header), tags$tbody(body)))
    })

    output$roles_dirty <- renderUI({
      cfg <- conf()$access_managemet
      # поля матрицы появляются после отрисовки: до этого нечего сравнивать
      if (length(cfg) == 0 || is.null(input[[matrix_input_id(1, 1)]])) return(NULL)

      state <- matrix_state()
      differs <- vapply(names(cfg), function(p) !setequal(state[[p]], cfg[[p]]), logical(1))
      dirty_badge(any(differs))
    })

    observeEvent(input$save_roles, {
      require_access_admin()
      old_matrix <- conf()$access_managemet
      new_matrix <- old_matrix
      state <- matrix_state()
      for (perm in names(old_matrix)) new_matrix[[perm]] <- state[[perm]]

      changes <- describe_config_changes(old_matrix, new_matrix)
      if (!nzchar(changes)) {
        showNotification("Права не менялись", type = "default")
        return()
      }

      save_config_section("access_managemet", new_matrix)
      write_action_log(user = auth$user()$login, func = "Access config change", session_id, value = changes)
      showNotification("✅ Права ролей обновлены. Новые сессии получат их сразу, открытым нужно обновить страницу.", type = "message")
    })

    # --- Безопасность входа -----------------------------------------------------------
    sec_keys <- names(SECURITY_FIELDS)

    security_field_ui <- function(key, sec, defaults) {
      meta <- SECURITY_FIELDS[[key]]
      id <- ns(paste0("sec_", key))
      value <- or_default(sec[[key]], defaults[[key]])

      switch(
        meta$type,
        int = div(
          class = "cfg-field",
          numericInput(id, meta$label, value = value, min = meta$min, max = meta$max, step = 1),
          div(class = "cfg-hint", paste0(meta$hint, ". От ", meta$min, " до ", meta$max, " (", meta$unit, ")."))
        ),
        enum = div(
          class = "cfg-field",
          selectInput(id, meta$label, choices = meta$choices, selected = value),
          div(class = "cfg-hint", meta$hint)
        ),
        flag = cfg_switch(id, meta$label, meta$hint, value)
      )
    }

    output$security_editor <- renderUI({
      sec <- conf()$security
      defaults <- default_section("security")

      tagList(
        div(class = "cfg-grid", lapply(SECURITY_MAIN, security_field_ui, sec = sec, defaults = defaults)),
        tags$details(
          class = "cfg-advanced",
          tags$summary("Дополнительно: длина паролей, автовход, хранение данных"),
          div(class = "cfg-grid", lapply(SECURITY_ADVANCED, security_field_ui, sec = sec, defaults = defaults))
        )
      )
    })

    security_inputs <- function() {
      stats::setNames(lapply(sec_keys, function(k) input[[paste0("sec_", k)]]), sec_keys)
    }

    output$security_dirty <- renderUI({
      sec <- conf()$security
      defaults <- default_section("security")
      values <- security_inputs()
      if (all(vapply(values, is.null, logical(1)))) return(NULL)

      differs <- vapply(sec_keys, function(k) {
        now <- normalize_security_value(k, values[[k]])
        was <- normalize_security_value(k, or_default(sec[[k]], defaults[[k]]))
        !is.null(values[[k]]) && !identical(now, was)
      }, logical(1))
      dirty_badge(any(differs))
    })

    observeEvent(input$save_security, {
      require_access_admin()
      values <- security_inputs()

      message_text <- validate_security_settings(values)
      if (!is.null(message_text)) {
        showNotification(paste("Не сохранено:", message_text), type = "error", duration = 10)
        return()
      }

      new_values <- stats::setNames(lapply(sec_keys, function(k) normalize_security_value(k, values[[k]])), sec_keys)
      old_security <- or_default(conf()$security, list())
      new_security <- utils::modifyList(old_security, new_values)

      changes <- describe_config_changes(old_security[sec_keys], new_security[sec_keys])
      if (!nzchar(changes)) {
        showNotification("Настройки входа не менялись", type = "default")
        return()
      }

      save_config_section("security", new_security)
      # новые сессии берут настройки отсюда; уже открытые работают с прежними
      auth_settings <<- build_auth_settings(new_security)
      write_action_log(user = auth$user()$login, func = "Security config change", session_id, value = changes)
      showNotification("✅ Настройки входа сохранены. Они применяются к новым сессиям.", type = "message")
    })

    # --- Журналы ----------------------------------------------------------------------
    output$logging_editor <- renderUI({
      log_nodes <- conf()$logging

      div(
        class = "cfg-list",
        lapply(names(log_nodes), function(key) {
          meta <- logging_meta(key)
          cfg_switch(ns(paste0("log_", key)), meta[1], meta[2], isTRUE(log_nodes[[key]]))
        })
      )
    })

    output$logging_dirty <- renderUI({
      log_nodes <- conf()$logging
      differs <- vapply(names(log_nodes), function(key) {
        v <- input[[paste0("log_", key)]]
        !is.null(v) && !identical(isTRUE(v), isTRUE(log_nodes[[key]]))
      }, logical(1))
      dirty_badge(any(differs))
    })

    observeEvent(input$save_logging, {
      require_access_admin()
      old_logging <- conf()$logging
      new_logging <- old_logging
      for (key in names(old_logging)) {
        v <- input[[paste0("log_", key)]]
        if (!is.null(v)) new_logging[[key]] <- isTRUE(v)
      }

      changes <- describe_config_changes(old_logging, new_logging)
      if (!nzchar(changes)) {
        showNotification("Настройки журналов не менялись", type = "default")
        return()
      }

      save_config_section("logging", new_logging)
      write_action_log(user = auth$user()$login, func = "Logging config change", session_id, value = changes)
      showNotification("✅ Настройки журналов обновлены и действуют сразу", type = "message")
    })

    # --- Базы данных ------------------------------------------------------------------
    file_paths <- reactiveVal(or_default(read_file_conf()$database_settings, conf()$database_settings))

    # какие базы сейчас реально используются (не меняются до перезапуска приложения)
    running_paths <- function() or_default(running_conf()$database_settings, conf()$database_settings)

    output$database_editor <- renderUI({
      db_version()
      paths <- file_paths()
      running <- running_paths()
      dirs <- unique(c(getwd(), dirname(vapply(c(paths, running), full_database_path, character(1)))))

      div(
        class = "cfg-list",
        lapply(names(paths), function(key) {
          meta <- database_meta(key)
          path <- full_database_path(paths[[key]])
          now <- full_database_path(or_default(running[[key]], ""))
          exists_now <- file.exists(path) && !dir.exists(path)

          status <- if (exists_now) {
            span(class = "cfg-ok", "✓ файл найден")
          } else if (key %in% DATABASE_ALLOW_NEW) {
            span(class = "cfg-warn", "➕ файла нет: при запуске будет создана новая база")
          } else {
            span(class = "cfg-bad", "⚠ файл не найден")
          }

          div(
            class = "cfg-field",
            selectInput(
              ns(paste0("db_", key)), meta[1],
              choices = database_choices(dirs, current = path), selected = path, width = "100%"
            ),
            # свой путь (в том числе ещё не созданной базы): если поле заполнено, берётся оно, а не выбор выше
            textInput(
              ns(paste0("dbtxt_", key)), NULL, value = "", width = "100%",
              placeholder = "Или введите другой путь, например D:/data/app_new.db"
            ),
            div(class = "cfg-hint", meta[2], " · ", status),
            if (!identical(path, now)) {
              div(class = "cfg-hint cfg-pending", "⏳ Вступит в силу после перезапуска приложения. Сейчас используется: ", now)
            }
          )
        })
      )
    })

    # итоговые пути из интерфейса: введённый вручную путь важнее выбранного в списке
    db_inputs <- function() {
      paths <- file_paths()
      stats::setNames(lapply(names(paths), function(k) {
        typed <- trimws(or_default(input[[paste0("dbtxt_", k)]], ""))
        chosen <- or_default(input[[paste0("db_", k)]], paths[[k]])
        full_database_path(if (nzchar(typed)) typed else chosen)
      }), names(paths))
    }

    output$database_dirty <- renderUI({
      paths <- file_paths()
      # поля появляются после отрисовки: до этого нечего сравнивать
      if (length(paths) == 0 || is.null(input[[paste0("db_", names(paths)[1])]])) return(NULL)

      values <- db_inputs()
      differs <- vapply(names(paths), function(key) !identical(values[[key]], full_database_path(paths[[key]])), logical(1))
      dirty_badge(any(differs))
    })

    pending_paths <- reactiveVal(NULL)

    observeEvent(input$save_database, {
      require_access_admin()
      old_paths <- stats::setNames(lapply(file_paths(), full_database_path), names(file_paths()))
      values <- db_inputs()

      changes <- describe_config_changes(old_paths, values)
      if (!nzchar(changes)) {
        showNotification("Пути к базам не менялись", type = "default")
        return()
      }

      message_text <- validate_database_paths(values, allow_new = DATABASE_ALLOW_NEW)
      if (!is.null(message_text)) {
        showNotification(paste("Не сохранено:", message_text), type = "error", duration = 10)
        return()
      }

      pending_paths(values)

      changed_keys <- names(values)[vapply(names(values), function(k) !identical(values[[k]], old_paths[[k]]), logical(1))]
      details <- lapply(changed_keys, function(k) {
        state <- if (file.exists(values[[k]])) "существующая база" else "новая база, будет создана при запуске"
        tags$li(strong(database_meta(k)[1]), ": ", values[[k]], " (", state, ")")
      })

      showModal(modalDialog(
        title = "Изменить пути к базам данных?",
        easyClose = FALSE,
        tags$ul(details),
        p("Изменение вступит в силу после перезапуска приложения, сейчас оно продолжает работать со старыми базами."),
        if (any(vapply(changed_keys, function(k) !file.exists(values[[k]]), logical(1)))) {
          p("В новой базе не будет пользователей: при входе нужно будет создать первого администратора. Старая база останется нетронутой.")
        },
        p("Чтобы вернуть всё назад, укажите прежние пути до перезапуска или восстановите конфиг из резервной копии."),
        footer = tagList(
          modalButton("Отмена"),
          actionButton(ns("confirm_database"), "Да, сохранить", class = "btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_database, {
      require_access_admin()
      values <- pending_paths()
      req(values)
      pending_paths(NULL)
      removeModal()

      # между окном подтверждения и нажатием файл мог измениться
      message_text <- validate_database_paths(values, allow_new = DATABASE_ALLOW_NEW)
      if (!is.null(message_text)) {
        showNotification(paste("Не сохранено:", message_text), type = "error", duration = 10)
        return()
      }

      old_paths <- stats::setNames(lapply(file_paths(), full_database_path), names(file_paths()))
      new_paths <- file_paths()
      for (key in names(values)) new_paths[[key]] <- values[[key]]

      # только файл: работающее приложение (общая conf и conf_rv) остаётся на прежних базах до перезапуска
      save_config_section("database_settings", new_paths, apply = FALSE)
      file_paths(new_paths)
      write_action_log(
        user = auth$user()$login, func = "Database config change", session_id,
        value = describe_config_changes(old_paths, values)
      )
      showNotification("✅ Пути сохранены в config.yaml и вступят в силу после перезапуска приложения", type = "message", duration = 10)
    })

    observeEvent(input$create_db_now, {
      require_access_admin()
      path <- full_database_path(or_default(input$newdb_path, ""))

      problem <- validate_new_database_path(path)
      if (!is.null(problem)) {
        showNotification(paste("Не создано:", problem), type = "error", duration = 10)
        return()
      }

      created <- tryCatch({ create_app_database(path); TRUE }, error = function(e) {
        showNotification(paste("Не создано:", conditionMessage(e)), type = "error", duration = 10)
        FALSE
      })
      if (!created) return()

      db_version(db_version() + 1)
      write_action_log(user = auth$user()$login, func = "Database config change", session_id, value = paste("создана база:", path))
      showNotification(
        paste0("✅ База создана: ", path, ". Выберите её в списке выше и сохраните пути, чтобы приложение перешло на неё после перезапуска."),
        type = "message", duration = 12
      )
    })

    # --- Резервные копии конфига -----------------------------------------------------------
    output$backups_editor <- renderUI({
      backups_version()
      backups <- list_config_backups()

      if (nrow(backups) == 0) {
        return(div(class = "cfg-hint", "Копий пока нет, первая появится перед ближайшим изменением настроек."))
      }

      labels <- vapply(seq_len(nrow(backups)), function(i) config_backup_label(backups$time[i], backups$size[i]), character(1))
      div(
        class = "cfg-field",
        selectInput(ns("backup_choice"), "Копия конфига (новые сверху)", choices = stats::setNames(backups$file, labels), width = "100%"),
        div(class = "cfg-hint", "Файлы лежат в папке ", CONFIG_BACKUP_DIR, " рядом с config.yaml и не попадают в git.")
      )
    })

    pending_restore <- reactiveVal(NULL)

    observeEvent(input$restore_backup, {
      require_access_admin()
      file <- input$backup_choice
      cfg <- read_config_backup(file)

      if (is.null(cfg)) {
        showNotification("Не восстановлено: копия не найдена или не читается", type = "error")
        return()
      }

      problem <- validate_restored_config(cfg, auth_settings$admin_role)
      if (!is.null(problem)) {
        showNotification(paste("Не восстановлено:", problem), type = "error", duration = 10)
        return()
      }

      pending_restore(file)
      showModal(modalDialog(
        title = "Восстановить конфиг из копии?",
        easyClose = FALSE,
        p(strong(file)),
        p("Вернутся права ролей, настройки входа и журналы. Пути к базам останутся текущими. Текущий конфиг перед этим тоже сохранится в копиях."),
        footer = tagList(
          modalButton("Отмена"),
          actionButton(ns("confirm_restore"), "Да, восстановить", class = "btn-warning")
        )
      ))
    })

    observeEvent(input$confirm_restore, {
      require_access_admin()
      file <- pending_restore()
      req(file)
      pending_restore(NULL)
      removeModal()

      cfg <- read_config_backup(file)
      problem <- if (is.null(cfg)) "копия не найдена или не читается" else validate_restored_config(cfg, auth_settings$admin_role)
      if (!is.null(problem)) {
        showNotification(paste("Не восстановлено:", problem), type = "error", duration = 10)
        return()
      }

      # администратор не должен потерять вкладку «Доступ»
      admin_role <- auth_settings$admin_role
      if ("Доступ" %in% names(cfg$access_managemet)) {
        cfg$access_managemet[["Доступ"]] <- unique(c(admin_role, cfg$access_managemet[["Доступ"]]))
      }

      current <- read_file_conf()
      backup_config()
      restored <- current
      for (section in c("access_managemet", "security", "logging")) restored[[section]] <- cfg[[section]]
      yaml::write_yaml(restored, "config.yaml")

      for (section in c("access_managemet", "security", "logging")) apply_running(section, restored[[section]])
      auth_settings <<- build_auth_settings(restored$security)
      backups_version(backups_version() + 1)

      write_action_log(user = auth$user()$login, func = "Config restore", session_id, value = file)
      showNotification("✅ Конфиг восстановлен из копии (пути к базам не менялись)", type = "message")
    })

    # --- Сброс к значениям по умолчанию (безопасность и журналы) -------------------------
    pending_reset <- reactiveVal(NULL)

    ask_reset <- function(section, title) {
      require_access_admin()
      pending_reset(section)
      showModal(modalDialog(
        title = title,
        easyClose = FALSE,
        p("Все значения этого раздела вернутся к значениям по умолчанию. Остальные настройки не изменятся."),
        footer = tagList(
          modalButton("Отмена"),
          actionButton(ns("confirm_reset"), "Да, сбросить", class = "btn-warning")
        )
      ))
    }

    observeEvent(input$reset_security, ask_reset("security", "Сбросить настройки входа?"))
    observeEvent(input$reset_logging, ask_reset("logging", "Сбросить настройки журналов?"))

    observeEvent(input$confirm_reset, {
      require_access_admin()
      section <- pending_reset()
      req(section %in% c("security", "logging"))
      pending_reset(NULL)
      removeModal()

      defaults <- default_section(section)
      old_section <- or_default(conf()[[section]], list())
      new_section <- utils::modifyList(old_section, defaults)

      changes <- describe_config_changes(old_section[names(defaults)], new_section[names(defaults)])
      if (!nzchar(changes)) {
        showNotification("Значения уже совпадают с настройками по умолчанию", type = "default")
        return()
      }

      save_config_section(section, new_section)
      if (section == "security") auth_settings <<- build_auth_settings(new_section)
      write_action_log(
        user = auth$user()$login,
        func = if (section == "security") "Security config change" else "Logging config change",
        session_id, value = paste("сброс:", changes)
      )
      showNotification("✅ Значения по умолчанию восстановлены", type = "message")
    })
  })
}
