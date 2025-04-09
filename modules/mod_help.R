# UI часть модуля
mod_help_ui <- function(id) {
  ns <- NS(id)
  
  # Возвращаем список из двух вкладок
  tabPanel(
      title = "README",
      div(class = "card",
          div(class = "card-header", "Help"),
          div(class = "card-body",
              includeMarkdown(here::here("README.md"))
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