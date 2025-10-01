# modules/mod_tab_ai_assistant.R

mod_tab_ai_assistant_ui <- function(id) {
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
      
      # Модульный UI shinychat (v0.2.x)
      chat_mod_ui(
        ns("simple_chat"), 
        messages = "👋 Привет!<br>Я умею писать код для работы со всеми внутренними источниками данных...<br>Чем могу помочь?"
      ),
      
      # Кнопка сброса
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
    
    client_rv <- reactiveVal(NULL)
    simple_chat <- NULL
    
    # --- Инициализация клиента и модуля ---
    new_client <- create_new_chat(user_role(), conf_rv())
    client_rv(new_client)
    
    # сохраняем объект shinychat
    simple_chat <- chat_mod_server("simple_chat", client = new_client)
    
    # --- Сброс чата ---
    observeEvent(input$reset_chat, {
      write_action_log(user = auth$user()$login,
                       func = 'AI Assistant Reset Chat',
                       session_id = session_id)
      
      if (!is.null(client_rv())) {
        client_rv()$set_turns(list())
      }
      
      if (!is.null(simple_chat)) {
        simple_chat$clear(clear_history = TRUE)
        showNotification("Контекст чата сброшен. Бот забыл всю предыдущую историю.",
                         type = "message", duration = 5)
      }
    })
    
    
    # --- Пользовательский ввод ---
    observeEvent(input$simple_chat_user_input, {
      req(client_rv())
      write_action_log(user = auth$user()$login,
                       func = 'AI Assistant',
                       session_id = session_id,
                       value = input$simple_chat_user_input)
      
      # асинхронный стриминг
      stream <- client_rv()$stream_async(input$simple_chat_user_input)
      if (!is.null(simple_chat)) {
        simple_chat$update_user_input(value = stream, submit = FALSE)
      }
    })
  })
}
