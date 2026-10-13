#' Серверные проверки прав доступа и валидация входных данных
#'
#' Скрытие кнопок и вкладок в интерфейсе не защищает от подделанных запросов,
#' поэтому права проверяются на сервере в каждом обработчике действия.

#' Есть ли у роли право на функцию
#'
#' @param role роль пользователя (из серверного состояния сессии)
#' @param permission название права, ключ в conf$access_managemet
#' @param conf конфиг приложения (список)
#'
#' @returns TRUE если роль входит в список ролей этого права
user_can <- function(role, permission, conf) {
  if (!is.character(role) || length(role) != 1 || is.na(role) || !nzchar(role)) return(FALSE)
  roles <- conf$access_managemet[[permission]]
  if (is.null(roles)) return(FALSE)
  role %in% roles
}

#' Проверка права в обработчике действия
#'
#' Вызывается первой строкой обработчика. Роль берётся из auth$user(), который
#' заполняется только серверным кодом входа. Если права нет, действие
#' прерывается и попытка записывается в лог.
#'
#' @param auth объект авторизации (список с reactiveVal user)
#' @param conf_rv reactiveVal с конфигом
#' @param permission название права
#' @param session_id id сессии для записи в лог
require_permission <- function(auth, conf_rv, permission, session_id = 'x0') {
  usr <- auth$user()

  if (!user_can(usr$role, permission, conf_rv())) {
    message("Отказано в доступе: пользователь=", usr$login %||% "anonymous", ", право=", permission)
    try(write_action_log(
      user       = usr$login %||% "anonymous",
      func       = 'Access denied',
      session_id = session_id,
      value      = permission
    ), silent = TRUE)
    showNotification("Недостаточно прав для этого действия", type = "error")
    req(FALSE)
  }

  invisible(TRUE)
}

#' Безопасно ли имя задачи для передачи в schtasks
#'
#' Отклоняет пустые значения, слишком длинные имена, кавычки и управляющие
#' символы, через которые можно подставить лишние аргументы.
#'
#' @param x имя задачи
#'
#' @returns TRUE если имя можно передавать в команду
is_safe_task_name <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) &&
    nzchar(x) && nchar(x) <= 260 &&
    !grepl('["\\x00-\\x1f]', x, perl = TRUE)
}

#' Существует ли задача в планировщике заданий Windows
#'
#' Используется там, где имя задачи приходит не из таблицы приложения, а,
#' например, от AI ассистента.
#'
#' @param x имя задачи
#'
#' @returns TRUE если задача найдена
task_exists_in_scheduler <- function(x) {
  if (!is_safe_task_name(x)) return(FALSE)

  out <- suppressWarnings(
    system2("schtasks", c("/query", "/tn", shQuote(x)), stdout = TRUE, stderr = TRUE)
  )

  status <- attr(out, "status")
  is.null(status) || identical(as.integer(status), 0L)
}
