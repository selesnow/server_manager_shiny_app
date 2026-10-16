#' Ручная блокировка пользователей
#'
#' Администратор может временно закрыть пользователю вход, не удаляя его
#' учётную запись. Блокировка хранится в колонке users.blocked (1 = заблокирован).
#' Заблокированный пользователь не может войти ни по паролю, ни по cookie
#' автовхода, его токены автовхода отзываются при блокировке, а уже открытая
#' сессия закрывается при ближайшей проверке (см. app.R).

#' Добавление колонки blocked в таблицу users (если её ещё нет)
#'
#' Безопасно вызывать при каждом запуске приложения, в том числе на работающих
#' базах dev и prod: существующие строки получают значение 0 (не заблокирован).
ensure_users_blocked_column <- function(conn) {
  cols <- DBI::dbGetQuery(conn, "SELECT name FROM pragma_table_info('users')")$name

  if (!"blocked" %in% cols) {
    DBI::dbExecute(conn, "ALTER TABLE users ADD COLUMN blocked INTEGER NOT NULL DEFAULT 0")
    message("В таблицу users добавлена колонка blocked")
  }

  invisible(TRUE)
}

#' Подготовка схемы при запуске приложения
#'
#' Ошибка не останавливает запуск, причина пишется в лог.
prepare_user_blocking <- function(conn) {
  tryCatch({
    ensure_users_blocked_column(conn)
    invisible(TRUE)
  }, error = function(e) {
    message("ВНИМАНИЕ: колонка blocked не подготовлена, ручная блокировка не работает: ", conditionMessage(e))
    invisible(FALSE)
  })
}

#' Заблокирован ли пользователь вручную
is_user_blocked <- function(conn, login) {
  r <- tryCatch(
    DBI::dbGetQuery(conn, "SELECT blocked FROM users WHERE login = ?", params = list(login)),
    error = function(e) data.frame(blocked = integer())
  )
  nrow(r) == 1 && isTRUE(r$blocked[1] == 1)
}

#' Можно ли пользователю продолжать работу в уже открытой сессии
#'
#' @returns FALSE если пользователя удалили или заблокировали
user_session_valid <- function(conn, login) {
  r <- tryCatch(
    DBI::dbGetQuery(conn, "SELECT blocked FROM users WHERE login = ?", params = list(login)),
    error = function(e) NULL
  )

  # при ошибке чтения базы сессию не рвём
  if (is.null(r)) return(TRUE)

  nrow(r) == 1 && !isTRUE(r$blocked[1] == 1)
}

#' Можно ли заблокировать пользователя
#'
#' @param conn соединение с app.db
#' @param login кого блокируем
#' @param acting_login кто блокирует (текущий администратор)
#'
#' @returns NULL если можно, иначе текст причины отказа
check_can_block_user <- function(conn, login, acting_login) {
  if (!is.character(login) || length(login) != 1 || is.na(login) || !nzchar(login)) {
    return("Пользователь не указан")
  }

  user <- DBI::dbGetQuery(conn, "SELECT login, role, blocked FROM users WHERE login = ?", params = list(login))
  if (nrow(user) == 0) return("Пользователь не найден")

  if (identical(login, acting_login)) return("Нельзя заблокировать самого себя")

  # нельзя остаться без активных администраторов
  if (identical(user$role[1], "admin")) {
    other_admins <- DBI::dbGetQuery(
      conn,
      "SELECT COUNT(*) AS n FROM users WHERE role = 'admin' AND blocked = 0 AND login <> ?",
      params = list(login)
    )$n
    if (other_admins == 0) return("Нельзя заблокировать последнего активного администратора")
  }

  NULL
}

#' Можно ли удалить пользователя
#'
#' Нельзя удалить самого себя и нельзя остаться без активных администраторов.
#'
#' @param conn соединение с app.db
#' @param login кого удаляем
#' @param acting_login кто удаляет (текущий администратор)
#'
#' @returns NULL если можно, иначе текст причины отказа
check_can_delete_user <- function(conn, login, acting_login) {
  if (!is.character(login) || length(login) != 1 || is.na(login) || !nzchar(login)) {
    return("Пользователь не указан")
  }

  user <- DBI::dbGetQuery(conn, "SELECT login, role FROM users WHERE login = ?", params = list(login))
  if (nrow(user) == 0) return("Пользователь не найден")

  if (identical(login, acting_login)) return("Нельзя удалить самого себя")

  # нельзя остаться без активных администраторов
  if (identical(user$role[1], "admin")) {
    other_admins <- DBI::dbGetQuery(
      conn,
      "SELECT COUNT(*) AS n FROM users WHERE role = 'admin' AND blocked = 0 AND login <> ?",
      params = list(login)
    )$n
    if (other_admins == 0) return("Нельзя удалить последнего активного администратора")
  }

  NULL
}

#' Удаление пользователя
#'
#' Кроме самой записи удаляет токены автовхода и неудачные попытки входа этого
#' логина. Без этого человек, которому позже достанется такой же логин, мог бы
#' войти по старой cookie прежнего владельца. Проверки допустимости
#' (check_can_delete_user) вызывающий код делает до этого.
#'
#' @returns число удалённых пользователей
delete_user_account <- function(conn, login) {
  n <- DBI::dbExecute(conn, "DELETE FROM users WHERE login = ?", params = list(login))

  if (n > 0) {
    DBI::dbExecute(conn, "DELETE FROM auth_sessions WHERE user_login = ?", params = list(login))
    tryCatch(clear_login_attempts(conn, login), error = function(e) NULL)
  }

  n
}

#' Блокировка и разблокировка пользователя
#'
#' При блокировке отзывает все токены автовхода пользователя. Проверки
#' допустимости (check_can_block_user) вызывающий код делает до этого.
#'
#' @returns число изменённых пользователей
set_user_blocked <- function(conn, login, blocked) {
  n <- DBI::dbExecute(
    conn,
    "UPDATE users SET blocked = ? WHERE login = ?",
    params = list(as.integer(isTRUE(blocked)), login)
  )

  if (n > 0 && isTRUE(blocked)) {
    DBI::dbExecute(conn, "UPDATE auth_sessions SET revoked = 1 WHERE user_login = ?", params = list(login))
  }

  n
}

#' Пользователь по токену автовхода (cookie)
#'
#' @returns data.frame из одной строки (login, role) или с нулём строк, заблокированные
#'   и удалённые пользователи не возвращаются
find_session_user <- function(conn, token_hash) {
  DBI::dbGetQuery(
    conn,
    "SELECT u.login, u.role
     FROM auth_sessions s
     JOIN users u ON u.login = s.user_login
     WHERE s.token_hash = ?
       AND s.revoked = 0
       AND s.expires_at > datetime('now')
       AND u.blocked = 0",
    params = list(token_hash)
  )
}

#' Таблица пользователей со статусами для вкладки «Доступ»
#'
#' @returns data.frame: login, role, Статус (текст) и state (blocked / locked / ok) для подсветки строк
users_overview <- function(conn) {
  users <- DBI::dbGetQuery(conn, "SELECT login, role, blocked FROM users ORDER BY rowid")

  if (nrow(users) == 0) {
    return(data.frame(login = character(), role = character(), `Статус` = character(),
                      state = character(), check.names = FALSE, stringsAsFactors = FALSE))
  }

  lock_left <- vapply(users$login, function(l) {
    tryCatch(login_lock_remaining(conn, l), error = function(e) 0L)
  }, numeric(1))

  state <- ifelse(users$blocked == 1, "blocked", ifelse(lock_left > 0, "locked", "ok"))
  status <- ifelse(
    state == "blocked", "Заблокирован администратором",
    ifelse(state == "locked", paste0("Вход заблокирован после неудачных попыток (ещё ", ceiling(lock_left / 60), " мин.)"), "")
  )

  out <- data.frame(login = users$login, role = users$role, `Статус` = status, state = state,
                    check.names = FALSE, stringsAsFactors = FALSE)
  out
}
