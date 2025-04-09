# UI часть модуля
mod_news_ui <- function(id) {
  ns <- NS(id)
  
  # Возвращаем список из двух вкладок
    tabPanel(
      title = "News",
      div(class = "card",
          div(class = "card-header", "News"),
          div(class = "card-body",
              includeMarkdown(here::here("NEWS.md"))
          )
      )
    )
}

# Серверная часть модуля
mod_news_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Серверная логика не требуется, так как содержимое статическое
  })
}