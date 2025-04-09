# UI часть модуля
mod_help_ui <- function(id) {
  ns <- NS(id)
  
  # Возвращаем список из двух вкладок
  tabPanel(
      title = "README",
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
                    ")
                   )
                  )
              )
          )
        )
      ),
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