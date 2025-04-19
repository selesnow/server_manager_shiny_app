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
    
    fluidRow(
      column(
        width = 12,
        div(class = "card",
            div(class = "card-body",
                h4("Видео мануал:", style = "margin-bottom: 20px;"),
                div(style = "text-align:center;",
                    tags$iframe(
                      src = "https://www.youtube.com/embed/RBn8vfgrVUo?si=59EpzUUFcyhD0eVe",
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
    
    # Блок с README.md
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
