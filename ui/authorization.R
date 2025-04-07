output$login_ui <- renderUI({
  if (!logged_in()) {
    fluidPage(
      # Добавляем стили для центрирования, увеличения ширины полей и применения тёмной темы
      tags$head(
        # Добавляем иконку для вкладки браузера
        tags$link(rel = "icon", type = "image/png", href = "favicon.png")
      ),
      tags$style(HTML("
        .login-container {
          display: flex;
          justify-content: center;
          align-items: center;
          height: 100vh;
          background-color: #333; /* Тёмный фон */
        }
        
        .login-container .form-container {
          width: 100%;
          max-width: 400px;  /* Максимальная ширина формы */
          padding: 20px;
          border: 1px solid #555;
          border-radius: 5px;
          background-color: #444;  /* Тёмный фон формы */
        }
        
        .login-container .form-container input, 
        .login-container .form-container button {
          width: 100%;  /* Делаем поля ввода и кнопки на всю ширину */
          padding: 10px;
          margin-bottom: 10px;
          background-color: #555;  /* Тёмный фон для полей */
          color: #f5f5f5;  /* Светлый текст */
          border-color: #666;  /* Тёмная граница */
        }
        
        .login-container .form-container button {
          background-color: #007bff; /* Кнопка входа с синим фоном */
        }
        
        #title-panel {
          text-align: center;
          color: #f5f5f5; /* Светлый текст заголовка */
        }
        
        .login-container .form-container input:focus, 
        .login-container .form-container button:focus {
          outline: none;  /* Убираем обводку при фокусе */
          border-color: #007bff; /* Синий цвет границы при фокусе */
        }
        
        /* Заголовок 'Авторизация' в тёмной теме */
        #title-panel {
          font-size: 24px;
          margin-bottom: 20px;
        }
      ")),
      
      # Контейнер с классом для выравнивания
      div(class = "login-container",
          fluidRow(
            column(12, class = "form-container",
                   # Заголовок "Авторизация" по центру
                   tags$h2(id = "title-panel", "Авторизация"),
                   textInput("login", "Логин"),
                   passwordInput("password", "Пароль"),
                   actionButton("login_btn", "Войти"),
                   textOutput("login_message")
            )
          )
      )
    )
  }
})