library(purrr)
library(tools)
library(stringr)
library(glue)

extract_docs <- function(man_path) {
  rd_files <- list.files(man_path, pattern = "\\.Rd$", full.names = TRUE)
  
  result <- map_chr(rd_files, function(file) {
    rd <- tryCatch(
      tools::parse_Rd(file),
      error = function(e) {
        message(glue("Ошибка в файле {file}: {e$message}"))
        return("")
      }
    )
    
    if (identical(rd, "")) return("")
    
    # Название функции
    name_sec <- keep(rd, ~ attr(.x, "Rd_tag") == "\\name")
    name <- if (length(name_sec) > 0) {
      str_trim(as.character(name_sec[[1]]))
    } else {
      basename(file)
    }
    
    # Title функции
    title_sec <- keep(rd, ~ attr(.x, "Rd_tag") == "\\title")
    title <- if (length(title_sec) > 0) {
      str_squish(paste(as.character(title_sec[[1]]), collapse = " "))
    } else {
      "(Нет title)"
    }
    
    # Аргументы
    args_section <- keep(rd, ~ attr(.x, "Rd_tag") == "\\arguments")
    if (length(args_section) > 0) {
      args_items <- keep(args_section[[1]], ~ attr(.x, "Rd_tag") == "\\item")
      args_text_vec <- map(args_items, function(arg_item) {
        arg_name <- str_trim(paste(as.character(arg_item[[1]]), collapse = " "))
        arg_desc <- str_squish(paste(as.character(arg_item[[2]]), collapse = " "))
        paste0("    * ", arg_name, " - ", arg_desc)
      })
      args_text <- if (length(args_text_vec) > 0) {
        paste(args_text_vec, collapse = "\n")
      } else {
        "    * (Нет аргументов)"
      }
    } else {
      args_text <- "    * (Нет аргументов)"
    }
    
    glue("* {name}() - {title}\n{args_text}\n")
  })
  
  paste(result, collapse = "\n")
}


# Пример использования:
cat(extract_docs("C:/scripts/alsey/netpeak_core/nc_analytics_team/internal_packages/n1/man"))
