# modules/mod_tab_ai_assistant.R

mod_tab_ai_assistant_ui <- function(id, messages = NULL) {
  ns <- NS(id)
  
  tabPanel(
    "AI Ассистент",
    bslib::page_fluid(
      tags$details(
        class = "stats-description",
        style = "margin-bottom: 20px; cursor: pointer; outline: none;",
        tags$summary(
          style = "font-weight: 600; color: var(--primary); font-size: 1.1em; list-style: none; display: flex; align-items: center; gap: 8px;",
          icon("info-circle"),
          "О возможностях ассистента (нажмите, чтобы развернуть/свернуть справку)"
        ),
        div(style = "margin-top: 15px; cursor: default;",
            HTML(glue::glue(
              "Этот AI ассистент поможет вам в разработке R кода для запроса данных из наших внутренних сервисов с использованием наших внутренних пакетов:<Br>",
              "<ul>",
              "<li><code>rpup</code> - Пакет для работы с базой ПУПа</li>
               <li><code>n1</code> - Пакет для работы с N1</li>
               <li><code>pfworker</code> - Пакет для работы с Планфикс</li>
               <li><code>segments</code> - Пакет для определения сегментов по списаниям или проект-услугам</li>
               <li><code>serviceaccounts</code> - Пакет для авторизации в Google сервисах через сервисные аккаунты</li>
               <li><code>alspy</code> - Python пакет для работы с базой ПУПа</li>",
              "</ul>",
              "<Br>Так же он умеет работать с задачами на сервере аналитики, бот умеет:<Br>",
              "<ul>",
              "<li>Искать нужную задачу по вашему описанию</li>
               <li>По названию задачу давать вам информацию о ней из планировщика заданий</li>
               <li>По названию задачу читать скрипт который она запускает, и читать лог его последнего выполнения</li>
               <li>Быстро проверять есть ли задачи работа которых была оставлена ошибкой</li>
               <li>Запускать задачи на сервере</li>
               <li>Активировать и деактивировать задачи в планировщике заданий",
              "</ul>",
              "<Br>Умеет запрашивать данные из Планфикса:<Br>",
              "<ul>",
              "<li>Выводить список задач по спринтам аналитиков по любому месяцу</li>
               <li>Получать информацию по любой задаче из планфикса по ссылке</li>
               <li>Выводить информацию по юнит экономике за любой месяц</li>",
              "</ul>",
              "<Br><Br>Более подробно ознакомится с возможностями данного AI Ассистента можно в <a href='https://youtu.be/sQRPMJYIxMA' target='_blank'>этом видео</a>."
            ))
        )
      ),
      
      chat_ui(
        id = ns("simple_chat"),
        greeting = if (!is.null(messages)) htmltools::HTML(messages) else NULL,
        icon_assistant = TRUE,
        submit_key = "enter",
        allow_attachments = TRUE,
        drawer = FALSE
      ),
      
      div(style = "text-align: center; margin-top: 15px; margin-bottom: 10px;",
          actionButton(
            ns("reset_chat_btn"), 
            "Очистить чат", 
            icon = icon("trash"), 
            class = "btn-danger btn-sm",
            style = "padding: 6px 16px !important; font-size: 0.9em; border-radius: 8px !important;"
          )
      )
    )
  )
}


mod_tab_ai_assistant_server <- function(id,
                                        auth,
                                        user_role,
                                        conf_rv,
                                        session_id,
                                        active = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # хранилища
    client_rv <- reactiveVal(NULL)        # ellmer::Chat
    simple_chat_rv <- reactiveVal(NULL)   # объект, возвращаемый chat_server()
    
    # ----- Инициализация клиента и chat_server -----
    # делаем создание клиента внутри реактивного обработчика (чтобы не дергать user_role() вне реактивного контекста)
    observeEvent(list(user_role(), conf_rv()), {
      new_client <- create_new_chat(user_role(), conf_rv())  # должен вернуть ellmer::Chat
      client_rv(new_client)
      
      # Получаем текущего пользователя для разделения истории диалогов
      usr_login <- "anonymous"
      try({
        u <- auth$user()
        if (!is.null(u) && nzchar(u$login)) usr_login <- u$login
      }, silent = TRUE)
      
      # Инициализация папки для хранения истории чатов
      history_dir <- here::here("chathistory")
      if (!dir.exists(history_dir)) {
        dir.create(history_dir, recursive = TRUE)
      }
      
      # Инициализируем chat_server с поддержкой истории по каждому пользователю
      sc <- chat_server(
        id = "simple_chat",
        client = new_client,
        history = history_options(
          restore_mode = "none", # Начинаем сессию всегда с чистого листа, не подгружая прошлый чат при старте
          store = FileConversationStore$new(dir = history_dir),
          scope = usr_login # Разделяем историю по пользователям
        )
      )
      
      # Функция для надежной регистрации/синхронизации Slash-команд с фронтендом
      register_cmds <- function() {
        tryCatch({
          # 1) /failed - быстрая проверка упавших задач
          sc$slash_command(
            name = "failed",
            description = "Найти задачи в планировщике, завершившиеся ошибкой",
            handler = function() {
              failed_tasks <- get_failed_tasks()
              if (identical(failed_tasks, "Все задачи выполнены успешно!")) {
                sc$append("🎉 Все задачи в планировщике Windows работают без ошибок!")
              } else {
                sc$append(paste0("⚠️ **Найдены упавшие задачи:**\n\n", failed_tasks))
              }
            },
            echo = TRUE,
            force = TRUE
          )
          
          # 2) /help - справочное меню по внутренним пакетам
          sc$slash_command(
            name = "help",
            description = "Показать справку по внутренним пакетам аналитики",
            handler = function() {
              sc$append(paste0(
                "### 📚 Доступные внутренние R-пакеты:\n\n",
                "- `rpup` — Основной пакет для подключения и запроса данных из базы данных ПУПа.\n",
                "- `pfworker` — Пакет для работы со всеми возможностями API Планфикса.\n",
                "- `n1` — Пакет для работы с HR ERP-системой N1 компании.\n",
                "- `segments` — Пакет автоматического определения сегментов по списаниям или проект-услугам.\n",
                "- `serviceaccounts` — Пакет авторизации и работы с Google Sheets / Google Drive через сервисные аккаунты.\n\n",
                "Вы можете задавать любые вопросы по использованию этих пакетов, ассистент умеет генерировать готовый Tidyverse-код с их использованием."
              ))
            },
            echo = TRUE,
            force = TRUE
          )
          
          # 3) /new - начать новый диалог (удобный аналог сброса чата)
          sc$slash_command(
            name = "new",
            description = "Начать новый диалог (сохраняет текущий и очищает экран)",
            handler = function() {
              tryCatch({
                sc$new_chat()
                showNotification("Начат новый диалог. Старый сохранен в истории.", type = "message", duration = 4)
              }, error = function(e) {
                showNotification(paste("Ошибка при создании нового чата:", conditionMessage(e)), type = "error")
              })
            },
            echo = FALSE,
            force = TRUE
          )
          
          # Форсируем отправку списка истории бесед на клиент для решения проблемы race condition при restore_mode = "none"
          ctrl <- shinychat:::get_session_chat_bookmark_info(session, "simple_chat.history-controller")
          if (!is.null(ctrl)) {
            ctrl$send_history_update()
          }
          
          message("[AI module] Slash commands and conversation history synchronized with client successfully")
        }, error = function(e) {
          message("[AI module] Error during slash command/history sync: ", conditionMessage(e))
        })
      }
      
      # Первичное выполнение при старте
      register_cmds()
      
      # Создаем периодический самовосстанавливающийся таймер для автоматической синхронизации команд (каждые 4 секунды)
      # Это решает любые проблемы с ленивой загрузкой вкладок, гонкой условий в JS и рендерингом DOM
      sync_timer <- reactiveTimer(4000)
      observe({
        sync_timer()
        register_cmds()
      })
      
      simple_chat_rv(sc)
      message("[AI module] chat_server 0.5.0 initialized with history and slash commands")
    }, ignoreInit = FALSE, once = TRUE)
    
    
    # ----- Подписки на last_input / last_turn (выполняются когда simple_chat готов) -----
    # Создаём подписки один раз после того как simple_chat_rv() не NULL
    observeEvent(simple_chat_rv(), {
      
      sc <- simple_chat_rv()
      if (is.null(sc)) return()
      
      # Обработка ввода пользователя — логируем ввод
      observeEvent(sc$last_input(), {
        
        # Получаем текст, который ввёл пользователь
        user_text <- sc$last_input()
        
        # Если это сложный объект (с вложениями), извлекаем текст из него
        if (is.list(user_text)) {
          text_parts <- sapply(user_text, function(part) {
            if (is.character(part)) return(part)
            if (is.list(part) && !is.null(part$text)) return(part$text)
            if (inherits(part, "S7_object") && "text" %in% names(part)) return(part@text)
            return("")
          })
          user_text <- paste(text_parts, collapse = "\n")
        }
        
        # Безопасно взятие логина
        usr_login <- NULL
        try({
          u <- auth$user()
          if (!is.null(u) && nzchar(u$login)) usr_login <- u$login
        }, silent = TRUE)
        
        # Логируем ввод (теперь срабатывает)
        tryCatch({
          write_action_log(
            user = usr_login %||% "unknown",
            func = 'AI Assistant',
            session_id = session_id,
            value = user_text
          )
          
          write_ai_chat_log(
            user       = usr_login %||% "unknown",
            session_id = session_id,
            role       = 'user', 
            message    = user_text
          )
          
          message("[AI module] logged user input: ", substr(user_text, 1, 200))
        }, error = function(e) {
          message("[AI module] write_action_log error: ", conditionMessage(e))
        })
      }, ignoreNULL = TRUE)
      
      
      # Optionally: наблюдать ответ ассистента (последний turn)
      observeEvent(sc$last_turn(), {
        assistant_turn <- sc$last_turn()
        
        text_content <- ""
        try({ text_content <- as.character(assistant_turn@text) }, silent = TRUE)
        if (!nzchar(text_content)) {
          try({ text_content <- as.character(assistant_turn$text) }, silent = TRUE)
        }
        if (!nzchar(text_content)) {
          try({ text_content <- as.character(assistant_turn) }, silent = TRUE)
        }
        
        write_ai_chat_log(
          user       = auth$user()$login,
          session_id = session_id,
          role       = 'ai', 
          message    = text_content
        )
        message("[AI module] assistant last_turn length: ", nchar(text_content))
      }, ignoreNULL = TRUE)
      
    }, once = TRUE) # настройка подписок один раз
    
    # ----- Обработчик кнопки нового диалога в тулбаре чата -----
    observeEvent(input$reset_chat_btn, {
      usr_login <- tryCatch({ auth$user()$login }, error = function(e) "anonymous")
      write_action_log(user = usr_login %||% "unknown",
                       func = 'AI Assistant Clear Chat Button',
                       session_id = session_id)

      # Нативно вызываем метод начала нового чата в контроллере shinychat
      sc <- simple_chat_rv()
      if (!is.null(sc)) {
        tryCatch({
          sc$new_chat()
          showNotification("Начат новый диалог. Предыдущий сохранён в истории.", type = "message", duration = 4)
        }, error = function(e) {
          try({ sc$clear() }, silent = TRUE)
        })
      }
    })

  })
}