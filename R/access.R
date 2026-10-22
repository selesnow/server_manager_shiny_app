#' Серверные проверки прав доступа и валидация входных данных
#'
#' Скрытие кнопок и вкладок в интерфейсе не защищает от подделанных запросов,
#' поэтому права проверяются на сервере в каждом обработчике действия.
#' Сама проверка выполняется пакетом appauth (aa_can, aa_require), здесь тонкие
#' обёртки с прежними именами и сигнатурами, чтобы вызовы в модулях не менялись.

#' Источник прав из матрицы config.yaml (раздел access_managemet)
#'
#' @param conf_rv reactiveVal с конфигом: матрица читается при каждой проверке,
#'   поэтому изменения прав в редакторе действуют сразу, без перезапуска
conf_permissions <- function(conf_rv) {
  aa_permissions_from_matrix(function() conf_rv()$access_managemet)
}

#' Источник прав «только роль администратора»
#'
#' Нужен там, где доступ не зависит от матрицы (серверная часть вкладки «Доступ»):
#' иначе выдача права «Доступ» другой роли дала бы ей управление пользователями.
admin_only_provider <- function(user) {
  if (identical(user$role, auth_settings$admin_role)) "Доступ" else character(0)
}

#' Есть ли у роли право на функцию
#'
#' @param role роль пользователя (из серверного состояния сессии)
#' @param permission название права, ключ в conf$access_managemet
#' @param conf конфиг приложения (список)
#'
#' @returns TRUE если роль входит в список ролей этого права
user_can <- function(role, permission, conf) {
  aa_can(
    list(login = "", role = role),
    permission,
    aa_permissions_from_matrix(conf$access_managemet %||% list())
  )
}

#' Проверка права в обработчике действия
#'
#' Вызывается первой строкой обработчика. Роль берётся из auth$user(), который
#' заполняется только серверным кодом входа. Если права нет, действие
#' прерывается, пользователю показывается уведомление, попытка записывается
#' в лог (действие 'Access denied').
#'
#' @param auth объект авторизации (результат aa_auth_server)
#' @param conf_rv reactiveVal с конфигом
#' @param permission название права
#' @param session_id id сессии для записи в лог
require_permission <- function(auth, conf_rv, permission, session_id = 'x0') {
  aa_require(
    auth, permission, conf_permissions(conf_rv),
    on_event   = log_auth_event,
    settings   = auth_settings,
    session_id = session_id
  )
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
