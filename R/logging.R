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
  
  if (Sys.getenv('SM_ACTION_LOG')=="") {
    
      con <- dbConnect(SQLite(), "app.db")
      
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
  
  if (Sys.getenv('SM_ACTION_LOG')=="") {
    
    con <- dbConnect(SQLite(), "app.db")
    
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