#' Функция поиска и чтения логов
#'
#' @param task_to_run Какой файл запускается задачей
#' @param start_in В какой директории стартует скрипт
#'
#' @returns текст R кода
#' @export
#'
find_script <- function(task_to_run = '', start_in = NULL) {
  
  file_ext <- tools::file_ext(unique(trimws(task_to_run)))
  
  # Обработка R файла
  if (tolower(file_ext) == 'r') {
    r_file    <- strsplit(unique(task_to_run), split = ' ')[[1]] %>%
      .[length(.)]
    r_path <- str_glue('{start_in}\\{r_file}')
    
    r_file  <- readLines(unique(r_path)) %>%
      str_c(., collapse = '\n')
    
    return(r_file)
  }
  
  # Функция для формирования заголовка со временем
  format_file_info <- function(paths) {
    existing_paths <- paths[file.exists(paths)]
    
    if (length(existing_paths) == 0) return("Нет найденных .R файлов.")
    
    file_info <- file.info(existing_paths)
    
    header <- map2_chr(
      rownames(file_info),
      file_info$mtime,
      ~ str_glue("{.x} — последнее изменение: {format(.y, '%Y-%m-%d %H:%M:%S')}")
    ) %>% str_c(collapse = '\n')
    
    str_glue("Найденные логи:\n\n{header}\n\n")
  }
  
  # Обработка .bat файла
  if (tolower(file_ext) == 'bat') {
    
    bat_file  <- task_to_run %>%
      str_trim() %>%
      unique() %>% 
      if_else(is.na(start_in) || start_in == "N/A", ., str_glue("{start_in}\\{.}")) %>% 
      readLines() %>% 
      str_c(., collapse = '\n')
    
    r_files <- str_extract_all(bat_file, "C:\\\\[^\\s]+\\.R")[[1]] %>% unique()
    r_paths <- str_glue("{r_files}")
    
    # Заголовок со списком логов и временем изменения
    r_file <- format_file_info(r_paths)
    
    for (r_file in r_files) {
      r_path <- str_glue("{r_file}")
      
      if (file.exists(r_path)) {
        r_file <- str_c(
          r_file, 
          "--------------------------------------------------------->\n", 
          " -- ", r_file, "\n", 
          "-------->\n", 
          readLines(r_path) %>% str_c(collapse = '\n'), 
          "\n<---------------------------------------------------------\n\n"
        )
      }
    }
    return(r_file)
  }
  
  
  return('Текст скрипта не найден!')
}
