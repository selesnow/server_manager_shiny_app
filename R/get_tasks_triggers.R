library(xml2)
library(dplyr)
library(purrr)
library(lubridate)
library(stringr)

# помощник: парсит ISO-like время; учитывает Z / +offset -> UTC, иначе локальное
parse_time <- function(s) {
  if (is.na(s) || !nzchar(s)) return(as.POSIXct(NA))
  s2 <- trimws(s)
  if (grepl("Z$|[+-]\\d{2}:?\\d{2}$", s2)) {
    t <- tryCatch(ymd_hms(s2, tz = "UTC"), error = function(e) NA)
    if (!is.na(t)) return(with_tz(t, "Europe/Kyiv"))
  }
  tryCatch(ymd_hms(s2, tz = Sys.timezone()), error = function(e) NA)
}

# помощник: безопасно извлекает текст, xpath относительно node
find_text_local_node <- function(node, xpath) {
  if (is.null(node)) return(NA_character_)
  n <- xml_find_first(node, xpath)
  if (inherits(n, "xml_missing") || length(n) == 0) return(NA_character_)
  txt <- xml_text(n)
  if (!nzchar(txt)) return(NA_character_) else return(txt)
}

# основная функция: одна строка на каждый триггер; подробные детали в list-column `schedule`
get_task_triggers <- function(folder = "C:/Windows/System32/Tasks") {
  folder_norm <- normalizePath(folder, winslash = "/", mustWork = FALSE)
  files <- list.files(folder_norm, full.names = TRUE, recursive = TRUE)
  files <- files[file.exists(files) & !file.info(files)$isdir]
  if (length(files) == 0) return(tibble())
  
  map_dfr(files, function(file) {
    xml <- tryCatch(read_xml(file), error = function(e) NULL)
    if (is.null(xml)) return(NULL)
    
    file_norm <- normalizePath(file, winslash = "/")
    rel <- sub(paste0("^", folder_norm), "", file_norm)
    rel <- sub("^/*", "", rel)
    parts <- strsplit(rel, "/")[[1]]
    task_name <- tail(parts, 1)
    dir_parts <- if (length(parts) > 1) head(parts, -1) else character(0)
    task_path <- if (length(dir_parts) == 0) "\\" else paste0("\\", paste(dir_parts, collapse = "\\"))
    
    triggers <- xml_find_all(xml, ".//*[local-name()='Triggers']/*")
    if (length(triggers) == 0) return(NULL)
    
    map_dfr(triggers, function(tr) {
      trigger_type_raw <- xml_name(tr) %||% NA_character_
      
      start_raw <- xml_text(xml_find_first(tr, ".//*[local-name()='StartBoundary']"))
      end_raw   <- xml_text(xml_find_first(tr, ".//*[local-name()='EndBoundary']"))
      start_parsed <- if (nzchar(start_raw)) parse_time(start_raw) else as.POSIXct(NA)
      end_parsed   <- if (nzchar(end_raw)) parse_time(end_raw) else as.POSIXct(NA)
      
      repetition_interval <- find_text_local_node(tr, ".//*[local-name()='Repetition']/*[local-name()='Interval']")
      repetition_duration <- find_text_local_node(tr, ".//*[local-name()='Repetition']/*[local-name()='Duration']")
      
      schedule_type <- NA_character_
      schedule_struct <- list()
      
      if (tolower(trigger_type_raw) != "calendartrigger") {
        schedule_type <- tolower(sub("Trigger$", "", trigger_type_raw))
      } else {
        if (length(xml_find_all(tr, ".//*[local-name()='ScheduleByDay']")) > 0) {
          schedule_type <- "daily"
          days_interval <- find_text_local_node(tr, ".//*[local-name()='ScheduleByDay']/*[local-name()='DaysInterval']")
          schedule_struct <- list(days_interval = days_interval)
        } else if (length(xml_find_all(tr, ".//*[local-name()='ScheduleByWeek']")) > 0) {
          schedule_type <- "weekly"
          weeks_interval <- find_text_local_node(tr, ".//*[local-name()='ScheduleByWeek']/*[local-name()='WeeksInterval']")
          dow_nodes <- xml_find_all(tr, ".//*[local-name()='ScheduleByWeek']//*[local-name()='DaysOfWeek']/*")
          days_of_week <- if (length(dow_nodes) > 0) xml_name(dow_nodes) else character(0)
          schedule_struct <- list(
            weeks_interval = weeks_interval,
            days_of_week   = days_of_week
          )
        } else if (length(xml_find_all(tr, ".//*[local-name()='ScheduleByMonth']")) > 0) {
          schedule_type <- "monthly"
          dom_nodes <- xml_find_all(tr, ".//*[local-name()='ScheduleByMonth']//*[local-name()='DaysOfMonth']/*[local-name()='Day']")
          days_of_month <- if (length(dom_nodes) > 0) xml_text(dom_nodes) else character(0)
          months_nodes <- xml_find_all(tr, ".//*[local-name()='ScheduleByMonth']//*[local-name()='Months']/*")
          months <- if (length(months_nodes) > 0) xml_name(months_nodes) else character(0)
          schedule_struct <- list(
            days_of_month = days_of_month,
            months        = months
          )
        } else {
          if (nzchar(start_raw) && is.na(repetition_interval)) {
            schedule_type <- "once"
          } else {
            schedule_type <- "calendar_unknown"
          }
        }
      }
      
      schedule_details <- switch(
        schedule_type,
        "daily" = {
          details <- c()
          if (!is.null(schedule_struct$days_interval))
            details <- c(details, paste0("Интервал дней: ", schedule_struct$days_interval))
          if (!is.na(repetition_interval) && nzchar(repetition_interval))
            details <- c(details, paste0("Повтор: ", repetition_interval))
          paste(details, collapse = "<br>")
        },
        "weekly" = {
          details <- c()
          if (!is.null(schedule_struct$weeks_interval))
            details <- c(details, paste0("Интервал недель: ", schedule_struct$weeks_interval))
          if (length(schedule_struct$days_of_week) > 0)
            details <- c(details, paste0("Дни недели: ", paste(schedule_struct$days_of_week, collapse = ", ")))
          if (!is.na(repetition_interval) && nzchar(repetition_interval))
            details <- c(details, paste0("Повтор: ", repetition_interval))
          paste(details, collapse = "<br>")
        },
        "monthly" = {
          details <- c()
          if (length(schedule_struct$days_of_month) > 0)
            details <- c(details, paste0("Дни месяца: ", paste(schedule_struct$days_of_month, collapse = ", ")))
          if (length(schedule_struct$months) > 0)
            details <- c(details, paste0("Месяцы: ", paste(schedule_struct$months, collapse = ", ")))
          if (!is.na(repetition_interval) && nzchar(repetition_interval))
            details <- c(details, paste0("Повтор: ", repetition_interval))
          paste(details, collapse = "<br>")
        },
        "once" = {
          if (!is.na(start_parsed)) format(start_parsed, "%Y-%m-%d %H:%M:%S") else NA_character_
        },
        schedule_type
      )
      
      tibble(
        task_name           = stringr::str_remove(str_glue('{task_path}\\{task_name}'), '^\\\\'),
        trigger_type_raw    = trigger_type_raw,
        schedule_type       = schedule_type,
        start_raw           = ifelse(nzchar(start_raw), start_raw, NA_character_),
        start_boundary      = start_parsed %>% with_tz("Europe/Kyiv"),
        time                = format(start_boundary, '%H:%M:%S'),
        end_raw             = ifelse(nzchar(end_raw), end_raw, NA_character_),
        end_boundary        = end_parsed,
        repetition_interval = repetition_interval,
        repetition_duration = repetition_duration,
        schedule_details    = schedule_details,
        schedule            = list(schedule_struct)
      ) %>% 
        select(
          -c(
            'start_raw',
            'end_raw',
            'end_boundary',
            'schedule',
            'trigger_type_raw'
          )
        )
    })
  })
}
