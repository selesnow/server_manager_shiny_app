#' Функция поиска и чтения логов
#'
#' @param task_to_run Какой файл запускается задачей
#' @param start_in В какой директории стартует скрипт
#'
#' @returns текст Rout лога
#' @export
#'
task_has_log <- function(task_to_run = '', start_in = NULL) {
  
  file_ext <- tools::file_ext(unique(trimws(task_to_run)))
  
  # Обработка R файла
  if (tolower(file_ext) == 'r') {
    r_file    <- strsplit(unique(task_to_run), split = ' ')[[1]] %>%
      .[length(.)]
    rout_path <- str_glue('{start_in}\\{r_file}out')
    
    return(file.exists(rout_path[1]))
    
  }
  
  # Обработка .bat файла
  if (tolower(file_ext) == 'bat') {
    
    bat_file  <- task_to_run %>%
      str_trim() %>%
      unique() %>% 
      if_else(is.na(start_in) || start_in == "N/A" || str_starts(., 'C:'), ., str_glue("{start_in}\\{.}")) %>% 
      readLines() %>% 
      str_c(., collapse = '\n')
    
    r_files <- str_extract_all(bat_file, "C:\\\\[^\\s]+\\.R")[[1]] %>% unique()
    rout_paths <- str_glue("{r_files}out")
    
    # Заголовок со списком логов и временем изменения
    rout_file <- format_file_info(rout_paths)
    
    if (!is.null(rout_file)) {
      
      for (r_file in r_files) {
        rout_path <- str_glue("{r_file}out")
        
        if (file.exists(rout_path)) {
          return(TRUE)
        }
      }
      
    }
    
  }
  
  # Обработка .ps1 файла
  if (tolower(file_ext) == 'ps1') {
    
    ps1_file  <- task_to_run %>%
      str_remove_all('powershell |\\.\\\\') %>% 
      str_trim() %>%
      unique() %>% 
      if_else(is.na(start_in) || start_in == "N/A" || str_starts(., 'C:'), ., str_glue("{start_in}\\{.}")) %>% 
      readLines() %>% 
      str_c(., collapse = '\n')
    
    r_files <- str_extract_all(ps1_file, "C:\\\\[^\\s]+\\.R")[[1]] %>% unique()
    rout_paths <- str_glue("{r_files}out")
    
    # Заголовок со списком логов и временем изменения
    rout_file <- format_file_info(rout_paths)
    
    if (!is.null(rout_file)) {
      
      for (r_file in r_files) {
        rout_path <- str_glue("{r_file}out")
        
        if (file.exists(rout_path)) {
          return(TRUE)
        }
      }
    }
  }
  
  # Попытка найти лог-файл другими способами, с явной обработкой ошибок
  log_fallback <- tryCatch(
    {
      find_log_files(start_in)
    },
    error = function(e) {
      str_glue("Не удалось прочитать лог. Возможно, проблема с кодировкой или повреждённый файл.\n\nОшибка: {e$message}")
    }
  )
  
  return(log_fallback)
  
}

# дополнительный поиск файлов
find_log_files <- function(start_in, patterns = c("*.log", "*.txt"), recursive = TRUE) {
  files <- dir(start_in, pattern = str_c(patterns, collapse = "|"), recursive = recursive, full.names = TRUE)
  files <- files[file.exists(files)]
  
  # Исключаем файлы, если в пути к ним есть "venv" (регистр неважен)
  files <- files[!str_detect(tolower(files), "venv")]
  
  if (length(files) == 0) return(FALSE) else return(TRUE)
  
}
