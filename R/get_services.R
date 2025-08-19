#' Получение служб с описанием
#'
#' @returns Таблицу с информацией по службам
#' @export
#'
#' @examples
get_services <- function() {
  
  service_data <- system("sc query state= all", intern = TRUE)
  service_lines <- grep("SERVICE_NAME: Analytics", service_data, value = TRUE)
  service_names <- str_extract(service_lines, "Analytics[А-Яа-я\\w\\-_]+")
  
  tibble(Service = service_names) %>%
    rowwise() %>%
    mutate(
      Status = {
        status <- system(glue("sc query \"{Service}\""), intern = TRUE)
        state_line <- grep("STATE", status, value = TRUE)
        str_trim(str_replace(state_line, ".*STATE.*: ", ""))
      },
      DisplayName = {
        info <- system(glue("sc qc \"{Service}\""), intern = TRUE)
        display_line <- grep("DISPLAY_NAME", info, value = TRUE)
        if (length(display_line) > 0) {
          str_trim(str_remove(display_line, "DISPLAY_NAME *:"))
        } else {
          "Нет DisplayName"
        }
      },
      Description = {
        desc_info <- suppressWarnings(system(glue("nssm get \"{Service}\" Description"), intern = TRUE))
        if (length(desc_info) > 0 && desc_info != "") {
          str_trim(desc_info)
        } else {
          "Нет описания"
        }
      },
      AppDirectory = {
        dir_info <- suppressWarnings(system(glue("nssm get \"{Service}\" AppDirectory"), intern = TRUE))
        if (length(dir_info) > 0 && dir_info != "") {
          str_trim(dir_info)
        } else {
          "Нет описания"
        }
      },
      Client = {
        client_info <- suppressWarnings(system(glue("nssm get \"{Service}\" AppDirectory"), intern = TRUE))
        if (length(client_info) > 0 && client_info != "") {
          str_trim(client_info)
          str_split(client_info, pattern = '\\\\|/') %>% unlist() %>% .[4] %>% to_title_case()
        } else {
          "Нет описания"
        }
      },
      AppParameters = {
        param_info <- suppressWarnings(system(glue("nssm get \"{Service}\" AppParameters"), intern = TRUE))
        if (length(param_info) > 0 && param_info != "") {
          str_trim(param_info)
        } else {
          "Нет описания"
        }
      },
      PID = {
        status_ex <- system(glue("sc queryex \"{Service}\""), intern = TRUE)
        pid_line <- grep("PID", status_ex, value = TRUE)
        pid_val <- as.integer(str_extract(pid_line, "\\d+"))
        if (!is.na(pid_val) && pid_val > 0) pid_val else NA
      },
      StartTime = {
        if (!is.na(PID)) {
          start_info <- system(glue("wmic process where ProcessId={PID} get CreationDate"), intern = TRUE)
          creation_line <- start_info[2]
          if (!is.na(creation_line) && nchar(creation_line) >= 14) {
            as.character(as.POSIXct(strptime(substr(creation_line, 1, 14), format = "%Y%m%d%H%M%S")))
          } else {
            NA
          }
        } else {
          NA
        }
      }
    ) %>%
    ungroup() %>% 
    mutate(update_time = lubridate::with_tz(Sys.time(), "Europe/Kyiv"))
}
