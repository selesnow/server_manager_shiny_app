# modules/mod_tab_ai_assistant.R

mod_tab_ai_assistant_ui <- function(id, messages = NULL) {
  ns <- NS(id)
  
  tabPanel(
    "AI Ассистент",
    bslib::page_fluid(
      div(class = "stats-description",
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
             <li>По названию задачи читать скрипт который она запускает, и читать лог его последнего выполнения</li>
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
      ),
      
      # UI чата (initial messages можно оставить)
      chat_mod_ui(
        ns("simple_chat"),
        messages = messages#"👋 Привет!<br>Я умею писать код для работы со всеми внутренними источниками данных, такими как ПУП, N1, Планфикс, умею работать со скриптами на сервере аналитики, а так же запрашивать информацию о задачах из Планфикс.<Br><Br>Чем могу тебе помочь?"
      ),
      
      # кнопка сброса
      div(class = "chat-controls", style = "margin-top: 15px; text-align: center;",
          actionButton(ns("reset_chat"), "Сбросить чат",
                       icon = icon("refresh"), class = "btn-warning btn-sm"))
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
    simple_chat_rv <- reactiveVal(NULL)   # объект, возвращаемый chat_mod_server()
    
    # ----- Инициализация клиента и chat_mod_server -----
    # делаем создание клиента внутри реактивного обработчика (чтобы не дергать user_role() вне реактивного контекста)
    observeEvent(list(user_role(), conf_rv()), {
      new_client <- create_new_chat(user_role(), conf_rv())  # должен вернуть ellmer::Chat
      client_rv(new_client)
      
      # call chat_mod_server and keep the returned object
      sc <- chat_mod_server("simple_chat", client = new_client)
      simple_chat_rv(sc)
      
      message("[AI module] chat_mod_server initialized")
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
        # last_turn() обычно содержит текст ответа ассистента
        assistant_turn <- sc$last_turn()
        # можно логировать или триггерить дополнительные действия
        write_ai_chat_log(
          user       = auth$user()$login,
          session_id = session_id,
          role       = 'ai', 
          message    = as.character(sc$last_turn()@text)
        )
        message("[AI module] assistant last_turn length: ", nchar(as.character(sc$last_turn()@text)))
      }, ignoreNULL = TRUE)
      
    }, once = TRUE) # настройка подписок один раз
    
    # ----- Сброс чата -----
    observeEvent(input$reset_chat, {
      # логируем действие
      usr_login <- tryCatch({ auth$user()$login }, error = function(e) NULL)
      write_action_log(user = usr_login %||% "unknown",
                       func = 'AI Assistant Reset Chat',
                       session_id = session_id)
      
      # 1) очистить историю внутри ellmer::Chat (если API поддерживает)
      if (!is.null(client_rv())) {
        tryCatch({
          client_rv()$set_turns(list())   # очищаем internal history
          message("[AI module] client_rv() turns cleared")
        }, error = function(e) {
          message("[AI module] client_rv()$set_turns error: ", conditionMessage(e))
        })
      }
      
      # 2) очистить UI виджета
      sc <- simple_chat_rv()
      if (!is.null(sc)) {
        tryCatch({
          sc$clear()
          message("[AI module] simple_chat$clear called")
        }, error = function(e) {
          message("[AI module] simple_chat$clear error: ", conditionMessage(e))
        })
      }
      
      # 3) показать уведомление
      showNotification("Контекст чата сброшен. Бот забыл всю предыдущую историю.", type = "message", duration = 4)
      
    })

  })
}
