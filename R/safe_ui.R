#' Копирование текста в буфер обмена браузера
#'
#' Текст уходит в браузер как данные (обработчик `copyToClipboard` в app.R), а не
#' подставляется в JS-код, поэтому кавычки и скрипты в названии задачи не выполняются.
#'
#' @param session сессия Shiny
#' @param text одна непустая строка
clipboard_copy <- function(session, text) {
  if (!is.character(text) || length(text) != 1 || is.na(text)) return(invisible(FALSE))
  session$sendCustomMessage("copyToClipboard", text)
  invisible(TRUE)
}

#' Markdown сообщения AI-чата в HTML
#'
#' commonmark пропускает сырой HTML и ссылки `javascript:`, поэтому до разбора markdown
#' символ `<` заменяется на `&lt;` (теги выводятся текстом), а после разбора в `href`
#' и `src` остаются только http, https, mailto, якоря и относительные адреса.
ai_log_message_html <- function(x) {
  vapply(x, function(m) {
    m <- gsub("<", "&lt;", as.character(m), fixed = TRUE)
    html <- commonmark::markdown_html(m)
    gsub("(href|src)=\"(?!(https?://|mailto:|#|/))[^\"]*\"", "\\1=\"#\"", html, perl = TRUE, ignore.case = TRUE)
  }, character(1), USE.NAMES = FALSE)
}

#' Какие колонки таблицы логов AI-чата экранировать
#'
#' Колонка `message` уже HTML из markdown, остальные значения (пользователь, роль,
#' сессия) выводятся как текст: возвращает отрицательный индекс message для `DT::datatable(escape =)`.
ai_log_escape_columns <- function(df) {
  idx <- match("message", names(df))
  if (is.na(idx)) TRUE else -idx
}

#' Можно ли читать файл лога, пришедший от клиента
#'
#' Разрешены только файлы из списка, который собрало приложение для выбранной задачи;
#' путь от клиента напрямую не принимается.
log_file_allowed <- function(path, allowed) {
  is.character(path) && length(path) == 1 && !is.na(path) && path %in% unname(allowed)
}
