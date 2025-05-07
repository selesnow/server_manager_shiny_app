mod_help_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "README",
    
    # Верхний блок со статистикой и описанием
    fluidRow(
      column(
        width = 12,
        div(class = "card",
            div(class = "card-body",
                div(class = "stats-summary",
                    uiOutput(ns("overall_stats_summary"))
                ),
                div(class = "stats-description",
                    h4("О приложении:"),
                    HTML(str_glue("
                      <ul>
                        <li><b>Название:</b> {yaml::read_yaml(here::here('app_info.yml'))$name}</li>
                        <li><b>Версия:</b> {yaml::read_yaml(here::here('app_info.yml'))$version}</li>
                        <li><b>Краткое описание: </b>{yaml::read_yaml(here::here('app_info.yml'))$description}</li>
                      </ul>
                    "))
                )
            )
        )
      )
    ),
    
    # Видео мануал
    fluidRow(
      column(
        width = 12,
        div(class = "card",
            div(class = "card-body",
                h4("Видео мануал:", style = "margin-bottom: 20px;"),
                div(style = "text-align:center;",
                    tags$iframe(
                      src = "https://www.youtube.com/embed/Io25a8W6Gz8?si=tOFC7-ykzkldtPzW",
                      width = "560",
                      height = "315",
                      frameborder = "0",
                      allowfullscreen = NA
                    )
                )
            )
        )
      )
    ),
    
    # Установка сертификата
    fluidRow(
      column(
        width = 12,
        div(class = "card",
            div(class = "card-body",
                h4("Установка SSL-сертификата", style = "margin-bottom: 20px;"),
                HTML("
                  <p>Для безопасного подключения к приложению используется самоподписанный сертификат <code>server.crt</code>.</p>
                  <p>Его необходимо установить на ваше устройство, чтобы избежать предупреждений о небезопасном соединении.</p>
                  <p><b>Скачать сертификат:</b> <a href='server.crt' download>server.crt</a></p>
                  <p>Видеоинструкции по установке сертификата:</p>
                "),
                div(style = "display: flex; justify-content: space-around; flex-wrap: wrap;",
                    tags$div(
                      tags$h5("Windows", style = "text-align:center;"),
                      tags$iframe(
                        src = "https://www.youtube.com/embed/Zi1PggpUO1k",
                        width = "300",
                        height = "170",
                        frameborder = "0",
                        allowfullscreen = NA
                      )
                    ),
                    tags$div(
                      tags$h5("Android", style = "text-align:center;"),
                      tags$iframe(
                        src = "https://www.youtube.com/embed/S0A01beKRGU",
                        width = "300",
                        height = "170",
                        frameborder = "0",
                        allowfullscreen = NA
                      )
                    ),
                    tags$div(
                      tags$h5("MacOS", style = "text-align:center;"),
                      tags$iframe(
                        src = "https://www.youtube.com/embed/urOmsDIgQR4",
                        width = "300",
                        height = "170",
                        frameborder = "0",
                        allowfullscreen = NA
                      )
                    )
                )
            )
        )
      )
    ),
    
    # README.md
    fluidRow(
      column(
        width = 12,
        div(class = "card",
            div(class = "card-body",
                includeMarkdown(here::here("README.md"))
            )
        )
      )
    )
  )
}

# Серверная часть модуля
mod_help_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Серверная логика не требуется, так как содержимое статическое
  })
}