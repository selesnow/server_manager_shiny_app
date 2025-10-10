#' Запись лога использования функционала приложения
#'
#' @param func Имя используемой функции
#' @param user Кем используется
#'
#' @returns
#' @export
#'
write_action_log <- function(
  func,
  user       = auth$user(),
  session_id = 'x0',
  value      = NULL
) {
  
  if (conf$logging$action_log) {
    
      con <- dbConnect(SQLite(), conf$database_settings$app_data_base)
      
      log_row <- tibble(
        datetime   = as.character(lubridate::with_tz(Sys.time(), "Europe/Kyiv")),
        func_name  = func,
        user       = user,
        session_id = session_id,
        value      = value
      )
      
      dbWriteTable(con, 'action_log', log_row, append = TRUE)
      
      dbDisconnect(con)
      
  }
  
}

session_log <- function(
    session_id,
    user = auth$user(),
    action = 'start'
) {
  message('Состояние лога сессий ', conf$logging$session_log)
  if (conf$logging$session_log) {
    
    con <- dbConnect(SQLite(), conf$database_settings$app_data_base)
    
    log_row <- tibble(
      datetime   = as.character(lubridate::with_tz(Sys.time(), "Europe/Kyiv")),
      session_id = session_id,
      user       = user,
      action     = action
    )
    
    dbWriteTable(con, 'session_log', log_row, append = TRUE)
    
    dbDisconnect(con)
    
  }
  
}

error_log <- function(
      session_id = "x0",
      user       = "",
      error      = ""
) {
  
  if (conf$logging$error_log) {
    
    con <- dbConnect(SQLite(), conf$database_settings$app_data_base)
    
    log_row <- tibble(
      datetime   = as.character(lubridate::with_tz(Sys.time(), "Europe/Kyiv")),
      session_id = session_id,
      user       = user,
      error      = error
    )
    
    dbWriteTable(con, 'error_log', log_row, append = TRUE)
    
    dbDisconnect(con)
    
  }
  
} 

write_ai_chat_log <- function(
    user       = auth$user(),
    session_id = session_id,
    role       = c('user', 'ai'), 
    message
) {
  
  if (conf$logging$ai_chat_log) {
    
    con <- dbConnect(SQLite(), conf$database_settings$app_data_base)
    
    log_row <- tibble(
      datetime   = as.character(lubridate::with_tz(Sys.time(), "Europe/Kyiv")),
      session_id = session_id,
      user       = user,
      role       = role,
      message    = message
    )
    
    dbWriteTable(con, 'ai_chat_log', log_row, append = TRUE)
    
    dbDisconnect(con)
    
  }
  
}

write_find_in_files_log <- function(
    results,
    query,
    user_login,
    session_id
) {
  if (conf$logging$find_in_files_log) {
    con <- dbConnect(SQLite(), conf$database_settings$app_data_base)
    
    # Определяем новый ID поиска
    last_id <- dbGetQuery(con, "SELECT MAX(search_id) AS max_id FROM find_in_files_log")$max_id
    if (is.na(last_id)) last_id <- 0
    new_id <- last_id + 1
    
    # Готовим данные к записи
    log_tbl <- results %>%
      mutate(
        search_id  = new_id,
        session_id = session_id,
        user       = user_login,
        query      = query,
        datetime   = as.character(lubridate::with_tz(Sys.time(), "Europe/Kyiv"))
      ) %>%
      select(search_id, session_id, user, query, file, line, match, extensions, datetime)
    
    # Записываем
    dbWriteTable(con, 'find_in_files_log', log_tbl, append = TRUE)
    dbDisconnect(con)
  }
}

get_session_log <- function() {
  
  con <- dbConnect(SQLite(), conf$database_settings$app_data_base)
  
  session_log <- dbGetQuery(
    con,
    "
    SELECT 
      session_log.session_id,
      user,
      DATE(MIN(CASE WHEN action = 'start' THEN datetime END)) as date,
      DATETIME(MIN(CASE WHEN action = 'start' THEN datetime END)) AS start_time,
      DATETIME(MAX(CASE WHEN action = 'end'   THEN datetime END))   AS end_time,
      (strftime('%s', MAX(CASE WHEN action = 'end'   THEN datetime END))
       - strftime('%s', MIN(CASE WHEN action = 'start' THEN datetime END))
      ) AS duration_seconds,
      COALESCE(action_count, 0) as action_count
      
    FROM session_log
    
    LEFT JOIN (
      select session_id, count(id) as action_count
      from action_log
      GROUP BY session_id
    ) actions
    ON session_log.session_id = actions.session_id
    
    GROUP BY session_log.session_id, user
    ORDER BY start_time;

    "
  )
  
  dbDisconnect(con)
  
  return(session_log)
  
}

get_action_log <- function() {
  
  con <- dbConnect(SQLite(), conf$database_settings$app_data_base)
  
  action_log <- dbGetQuery(
    con,
    "
    SELECT 
    DATETIME(al.datetime) as datetime,
    al.user,
    al.session_id,
    a.tab,
    a.name AS action,
    al.value AS detail
    
    FROM action_log al
    LEFT JOIN actions a 
        ON al.func_name = a.key;

    "
  )
  
  dbDisconnect(con)
  
  return(action_log)
  
}

get_error_log <- function() {
  
  con <- dbConnect(SQLite(), conf$database_settings$app_data_base)
  
  error_log <- dbGetQuery(
    con,
    "SELECT * FROM error_log"
  )
  
  dbDisconnect(con)
  
  return(error_log)
  
}

format_seconds <- function(seconds) {
  h <- seconds %/% 3600
  m <- (seconds %% 3600) %/% 60
  s <- seconds %% 60
  sprintf("%02d:%02d:%02d", h, m, s)
}