#' Функция чтения README
#'
#' @param start_in Папка в которой будем искать README
#'
#' @returns HTML код отрендереного README файла
#' @export
#'
find_readme <- function(start_in = ".") {  
  # приоритет сначала README.Rmd, потом README.md
  files <- c("README.Rmd", "README.md")
  start_in <- str_remove(start_in, '\\\\R$|/R$|/R/$')
  full_paths <- here::here(start_in, files)
  existing_file <- full_paths[file.exists(full_paths)][1]  # первый найденный
  
  if (!length(existing_file) || is.na(existing_file)) {
    return("<p>README не найден!</p>")
  }
  
  # рендерим HTML (в ту же директорию)
  rendered_html <- rmarkdown::render(
    input = existing_file,
    output_format = "html_document",
    output_dir = start_in,
    quiet = TRUE
  )
  
  # читаем содержимое HTML-файла
  html <- readLines(rendered_html, warn = FALSE, encoding = "UTF-8")
  html_combined <- paste(html, collapse = "\n")
  
  return(html_combined)
}