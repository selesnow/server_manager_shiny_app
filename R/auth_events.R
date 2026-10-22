#' Связка приложения с пакетом авторизации appauth
#'
#' Пакет сам ничего не пишет в логи приложения, о событиях он сообщает одной
#' функции (хуку). Здесь она превращает события в записи лога действий
#' (write_action_log) и лога сессий (session_log) с прежними названиями действий,
#' чтобы старые отчёты и справочник actions продолжили работать.

#' Настройки авторизации из раздела security файла config.yaml
#'
#' Незаданные значения берутся из пакета (aa_settings). Опечатка в конфиге
#' (например, нулевой лимит или текст вместо числа) не должна ронять приложение:
#' тогда пишется предупреждение и применяются настройки по умолчанию.
#'
#' @param security список security из config.yaml (может быть NULL)
#'
#' @returns настройки для функций appauth
build_auth_settings <- function(security) {
  tryCatch(
    aa_settings_from_list(security, texts = aa_texts("ru")),
    error = function(e) {
      message("ВНИМАНИЕ: некорректный раздел security в config.yaml (", conditionMessage(e), "), использованы значения по умолчанию")
      aa_settings(texts = aa_texts("ru"))
    }
  )
}

# соответствие событий пакета и названий действий в логе (см. actions_catalog())
AUTH_EVENT_ACTIONS <- c(
  login_failed       = "Login failed",
  login_locked       = "Login locked",
  login_blocked_user = "Blocked login attempt",
  login_throttled    = "Login throttled",
  login_unlocked     = "Login unlock",
  first_admin_created = "User add",
  user_added         = "User add",
  user_removed       = "User remove",
  user_role_changed  = "User change role",
  password_reset     = "User password reset",
  password_changed   = "Password change",
  sessions_revoked   = "Sessions revoke",
  user_blocked       = "User block",
  user_unblocked     = "User unblock",
  session_forced_logout = "Forced logout",
  access_denied      = "Access denied"
)

#' Значение для колонки value лога
#'
#' В старых записях value это логин, над которым выполнено действие. Пакет
#' кладёт в value уточнения («логин (роль)», «логин: роль»), берём только логин.
auth_event_log_value <- function(event, user, value) {
  switch(
    event,
    login_failed = , login_locked = , login_blocked_user = , login_throttled = user,
    user_added = sub(" \\(.*\\)$", "", value),
    user_role_changed = sub(":.*$", "", value),
    first_admin_created = user,
    value
  )
}

#' Хук событий авторизации для пакета appauth
#'
#' @param event ключ события (aa_events())
#' @param user логин, от имени которого событие
#' @param session_id идентификатор сессии Shiny
#' @param value уточнение события
log_auth_event <- function(event, user, session_id, value) {
  # выход (по кнопке и принудительный): фиксируем конец сессии, пока пользователь известен
  if (event %in% c("logout", "session_forced_logout") && !identical(user, "anonymous")) {
    try(session_log(session_id = session_id, user = user, action = "end"), silent = TRUE)
  }

  func <- AUTH_EVENT_ACTIONS[event]
  if (is.na(func)) return(invisible(NULL))

  try(
    write_action_log(
      user       = user,
      func       = unname(func),
      session_id = session_id,
      value      = auth_event_log_value(event, user, value)
    ),
    silent = TRUE
  )

  invisible(NULL)
}
