#' Функция поиска и чтения логов
#'
#' @param task_to_run Какой файл запускается задачей
#' @param start_in В какой директории стартует скрипт
#'
#' @returns текст Rout лога
#' @export
#'
find_log <- function(task_to_run = NULL, start_in = NULL) {  # Исправление: null -> NULL
  
  file_ext <- tools::file_ext(unique(trimws(task_to_run)))
  
  if (tolower(file_ext) == 'r') {
    
    r_file    <- strsplit(unique(task_to_run), split = ' ')[[1]] %>% 
      .[length(.)]
    rout_path <- str_glue('{start_in}\\{r_file}out')
    
    rout_file  <- readLines(unique(rout_path)) %>% 
      str_c(., collapse = '\n')
    
    return(rout_file)
    
  }
  
  if (tolower(file_ext) == 'bat') {
    
    bat_file  <- readLines(unique(task_to_run)) %>% 
      str_c(., collapse = '\n')
    
    bat_file  <- str_glue(bat_file)
    
    r_file    <- strsplit(unique(bat_file), split = ' ')[[1]] %>% 
      .[length(.)]
    
    rout_path <- str_glue('{r_file}out')
    
    if (file.exists(rout_path)) {
      rout_file  <- readLines(unique(rout_path)) %>% 
        str_c(., collapse = '\n')
      
      return(rout_file)
    }
    
  }
  
  return('Лог не найден!')
  
}