#' Функция поиска и чтения логов
#'
#' @param task_to_run Какой файл запускается задачей
#' @param start_in В какой директории стартует скрипт
#'
#' @returns текст Rout лога
#' @export
#'
find_log <- function(task_to_run = '', start_in = NULL) {
  
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
          rout_file <- str_c(
            rout_file, 
            "--------------------------------------------------------->\n", 
            " -- ", r_file, "out\n", 
            "-------->\n", 
            readLines(rout_path) %>% str_c(collapse = '\n'), 
            "\n<---------------------------------------------------------\n\n"
          )
        }
      }
      
      return(rout_file)
      
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
          rout_file <- str_c(
            rout_file, 
            "--------------------------------------------------------->\n", 
            " -- ", r_file, "out\n", 
            "-------->\n", 
            readLines(rout_path) %>% str_c(collapse = '\n'), 
            "\n<---------------------------------------------------------\n\n"
          )
        }
      }
      
      return(rout_file)
    }
  }
  
  print('I am here, look 1')
  try(return(read_log_files(start_in)))
  print('I am here, look 2')
  return('Лог не найден!')
}

# дополнительный поиск файлов
read_log_files <- function(start_in, patterns = c("*.log", "*.txt"), recursive = TRUE) {
  files <- dir(start_in, pattern = str_c(patterns, collapse = "|"), recursive = recursive, full.names = TRUE)
  files <- files[file.exists(files)]
  
  # Исключаем файлы, если в пути к ним есть "venv" (регистр неважен)
  files <- files[!str_detect(tolower(files), "venv")]
  
  if (length(files) == 0) return("Нет логов.")
  
  files_info <- file.info(files)
  newest_file <- rownames(files_info)[which.max(files_info$mtime)]
  
  log_ctn <- readLines(newest_file, warn = FALSE, encoding = '1251') %>% str_c(collapse = '\n')
  log_header  <- format_file_info(files)
  
  str_c(log_header,
        "--------------------------------------------------------->\n", 
        " -- ", newest_file, "\n", 
        "-------->\n", 
        log_ctn, 
        "\n<---------------------------------------------------------\n\n")
}

# Функция для формирования заголовка со временем
format_file_info <- function(paths) {
  existing_paths <- paths[file.exists(paths)]
  
  if (length(existing_paths) > 0) {
    file_info <- file.info(existing_paths)
    
    header <- map2_chr(
      rownames(file_info),
      file_info$mtime,
      ~ str_glue("{.x} — последнее изменение: {format(.y, '%Y-%m-%d %H:%M:%S')}")
    ) %>% str_c(collapse = '\n')
    
    str_glue("Найденные логи:\n\n{header}\n\n")
  }
}
