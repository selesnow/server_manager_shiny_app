#' Функция поиска и чтения логов
#'
#' @param task_to_run Какой файл запускается задачей
#' @param start_in В какой директории стартует скрипт
#'
#' @returns текст Rout лога
#' @export
#'
find_log <- function(task_to_run = '', start_in = NULL) {  # Исправление: null -> NULL
  
  file_ext <- tools::file_ext(unique(trimws(task_to_run)))
  
  # Обработка R файла
  if (tolower(file_ext) == 'r') {
    
    r_file    <- strsplit(unique(task_to_run), split = ' ')[[1]] %>%
      .[length(.)]
    rout_path <- str_glue('{start_in}\\{r_file}out')
    
    rout_file  <- readLines(unique(rout_path)) %>%
      str_c(., collapse = '\n')
    
    return(rout_file)
    
  }
  
  # Обработка .bat файла
  if (tolower(file_ext) == 'bat') {
    
    bat_file  <- task_to_run %>%
                  str_trim() %>%
                  unique() %>% 
                  if_else(is.na(start_in) || start_in == "N/A", ., str_glue("{start_in}\\{.}")) %>% 
                  readLines() %>% 
                  str_c(., collapse = '\n')
    
    # Регулярное выражение для поиска всех .R файлов в .bat файле
    r_files <- str_extract_all(bat_file, "C:\\\\[^\\s]+\\.R")[[1]] %>% unique()
    
    rout_file <- ""
    
    for (r_file in r_files) {
      rout_path <- str_glue("{r_file}out")
      
      if (file.exists(rout_path)) {
        rout_file <- str_c(
          rout_file, 
          "--------------------------------------------------------->\n", 
          " -- ", r_file, "out\n", 
          "-------->\n", 
          readLines(rout_path) %>% str_c(collapse = '\n'), 
          "<--------\n", 
          "\n<---------------------------------------------------------\n"
          )
      }
    }
    return(rout_file)
  }
  
  # Обработка .ps1 файла
  if (tolower(file_ext) == 'ps1') {
    
    ps1_file  <- task_to_run %>%
                 str_remove_all('powershell ') %>% 
                 str_trim() %>%
                 unique() %>% 
                 if_else(is.na(start_in) || start_in == "N/A", ., str_glue("{start_in}\\{.}")) %>% 
                 readLines() %>% 
                 str_c(., collapse = '\n')
    
    # Регулярное выражение для поиска всех .R файлов в .ps1 файле
    r_files <- str_extract_all(ps1_file, "C:\\\\[^\\s]+\\.R")[[1]] %>% unique()
    
    rout_file <- ""
    
    for (r_file in r_files) {
      rout_path <-  str_glue("{r_file}out")
      
      if (file.exists(rout_path)) {
        rout_file <- str_c(
          rout_file, 
          "--------------------------------------------------------->\n", 
          " -- ", r_file, "out\n", 
          "-------->\n", 
          readLines(rout_path) %>% str_c(collapse = '\n'), 
          "<--------\n", 
          "\n<---------------------------------------------------------\n"
        )
      }
    }
    
    return(rout_file)
  }
  
  return('Лог не найден!')
}
