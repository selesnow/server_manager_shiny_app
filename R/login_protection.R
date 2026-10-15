#' Защита формы входа от подбора пароля
#'
#' Неудачные попытки входа считаются по введённому логину (независимо от того,
#' существует ли такой логин, поэтому по сообщениям нельзя узнать список
#' пользователей). Если за окно в lockout_minutes минут накопилось
#' max_failed_logins неудачных попыток, вход под этим логином блокируется на
#' lockout_minutes минут с момента последней неудачной попытки. Попытки во время
#' блокировки не записываются и блокировку не продлевают. Успешный вход
#' обнуляет счётчик. Дополнительно есть общий лимит неудачных попыток по всем
#' логинам за минуту, он останавливает перебор по многим логинам и рост таблицы.
#' Время хранится в UTC и сравнивается средствами SQLite.

# настройки по умолчанию, значения можно менять в разделе security файла config.yaml
LOGIN_DEFAULTS <- list(
  max_failed_logins            = 5,
  lockout_minutes              = 15,
  max_failed_logins_per_minute = 30
)

#' Настройки защиты входа из config.yaml (с запасными значениями)
login_security_settings <- function(cfg = NULL) {
  if (is.null(cfg)) cfg <- if (exists("conf", inherits = TRUE)) get("conf", inherits = TRUE) else list()

  pick <- function(name) {
    v <- suppressWarnings(as.numeric(cfg$security[[name]]))
    if (length(v) != 1 || is.na(v) || v < 1) LOGIN_DEFAULTS[[name]] else v
  }

  list(
    max_failed       = pick("max_failed_logins"),
    lockout_minutes  = pick("lockout_minutes"),
    max_per_minute   = pick("max_failed_logins_per_minute")
  )
}

#' Приведение логина к виду, в котором он хранится в таблице попыток
normalize_login <- function(x) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) return("")
  substr(tolower(trimws(x)), 1, 64)
}

#' Создание таблицы попыток входа (если её ещё нет)
#'
#' Безопасно вызывать при каждом запуске приложения, в том числе на уже
#' работающих базах dev и prod, существующие таблицы и данные не затрагиваются.
ensure_login_attempts_table <- function(conn) {
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS login_attempts (
      id           INTEGER PRIMARY KEY AUTOINCREMENT,
      login        TEXT NOT NULL,
      success      INTEGER NOT NULL,
      attempted_at TEXT NOT NULL DEFAULT (datetime('now'))
    )
  ")
  DBI::dbExecute(conn, "
    CREATE INDEX IF NOT EXISTS idx_login_attempts_login ON login_attempts (login, attempted_at)
  ")
  invisible(TRUE)
}

#' Запись попытки входа
record_login_attempt <- function(conn, login, success) {
  login <- normalize_login(login)
  if (!nzchar(login)) return(invisible(FALSE))

  DBI::dbExecute(
    conn,
    "INSERT INTO login_attempts (login, success) VALUES (?, ?)",
    params = list(login, as.integer(isTRUE(success)))
  )
  invisible(TRUE)
}

#' Сколько секунд осталось до конца блокировки логина
#'
#' @returns 0 если вход разрешён
login_lock_remaining <- function(conn, login, max_failed = NULL, lockout_minutes = NULL) {
  s <- login_security_settings()
  if (is.null(max_failed))      max_failed      <- s$max_failed
  if (is.null(lockout_minutes)) lockout_minutes <- s$lockout_minutes

  login <- normalize_login(login)
  if (!nzchar(login)) return(0L)

  window <- sprintf("-%d seconds", as.integer(lockout_minutes * 60))

  # неудачные попытки после последнего успешного входа и внутри окна
  r <- DBI::dbGetQuery(
    conn,
    "SELECT COUNT(*) AS n,
            MAX(strftime('%s', attempted_at)) AS last_fail,
            CAST(strftime('%s', 'now') AS INTEGER) AS now_ts
     FROM login_attempts
     WHERE login = ?
       AND success = 0
       AND attempted_at > datetime('now', ?)
       AND id > COALESCE(
             (SELECT MAX(id) FROM login_attempts WHERE login = ? AND success = 1),
             0)",
    params = list(login, window, login)
  )

  if (r$n < max_failed) return(0L)

  remaining <- as.integer(r$last_fail) + as.integer(lockout_minutes * 60) - r$now_ts
  max(0L, as.integer(remaining))
}

#' Превышен ли общий лимит неудачных попыток входа по всем логинам за минуту
login_global_throttled <- function(conn, max_per_minute = NULL) {
  if (is.null(max_per_minute)) max_per_minute <- login_security_settings()$max_per_minute

  n <- DBI::dbGetQuery(
    conn,
    "SELECT COUNT(*) AS n FROM login_attempts
     WHERE success = 0 AND attempted_at > datetime('now', '-60 seconds')"
  )$n

  n >= max_per_minute
}

#' Снятие блокировки логина (удаляет его неудачные попытки)
#'
#' @returns число удалённых записей
clear_login_attempts <- function(conn, login) {
  login <- normalize_login(login)
  if (!nzchar(login)) return(0L)

  DBI::dbExecute(
    conn,
    "DELETE FROM login_attempts WHERE login = ? AND success = 0",
    params = list(login)
  )
}

#' Очистка устаревших данных безопасности
#'
#' Удаляет попытки входа старше суток и просроченные или отозванные токены
#' автовхода старше 30 дней.
#'
#' @returns список с числом удалённых строк
cleanup_security_tables <- function(conn) {
  attempts <- DBI::dbExecute(
    conn,
    "DELETE FROM login_attempts WHERE attempted_at < datetime('now', '-1 day')"
  )

  sessions <- DBI::dbExecute(
    conn,
    "DELETE FROM auth_sessions
     WHERE expires_at < datetime('now', '-30 days')
        OR (revoked = 1 AND created_at < datetime('now', '-30 days'))"
  )

  list(login_attempts = attempts, auth_sessions = sessions)
}

#' Подготовка схемы и уборка при запуске приложения
#'
#' Ошибки не останавливают запуск: без таблицы защита входа просто не работает
#' (см. fail-open в модуле авторизации), а причина пишется в лог.
prepare_login_protection <- function(conn) {
  tryCatch({
    ensure_login_attempts_table(conn)
    cleaned <- cleanup_security_tables(conn)
    if (cleaned$login_attempts + cleaned$auth_sessions > 0) {
      message("Очистка безопасности: попыток входа удалено ", cleaned$login_attempts,
              ", устаревших токенов удалено ", cleaned$auth_sessions)
    }
    invisible(TRUE)
  }, error = function(e) {
    message("ВНИМАНИЕ: защита входа не подготовлена: ", conditionMessage(e))
    invisible(FALSE)
  })
}
