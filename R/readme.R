#' README.md в HTML для вкладки README
#'
#' В файле картинки лежат по пути `www/readme/имя.png`: так они видны и в репозитории
#' (GitLab), и работают как относительные ссылки. Приложение отдаёт папку `www` с корня,
#' поэтому для вкладки путь сокращается до `readme/имя.png`. Картинки не встраиваются в
#' страницу (иначе каждый вход тянул бы сотни килобайт base64), браузер берёт их отдельными
#' файлами и кеширует.
#'
#' @param path путь к README.md
#'
#' @returns HTML для вставки в интерфейс
readme_html <- function(path = "README.md") {
  txt <- readLines(path, encoding = "UTF-8", warn = FALSE)
  txt <- gsub("www/readme/", "readme/", txt, fixed = TRUE)

  shiny::HTML(markdown::mark(
    text = txt, format = "html", options = list(embed_resources = FALSE), meta = list(), template = FALSE
  ))
}
