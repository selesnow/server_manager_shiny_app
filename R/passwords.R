#' Работа с паролями пользователей приложения
#'
#' Пароли хранятся только в виде хеша scrypt (пакет sodium), хеш содержит соль
#' и параметры и начинается с префикса `$7$`.

# ограничения на новые пароли
PASSWORD_MIN_LENGTH <- 6
PASSWORD_MAX_LENGTH <- 128

#' Хеширование пароля
#'
#' @param password пароль в открытом виде
#'
#' @returns строка с хешем
hash_password <- function(password) {
  stopifnot(is.character(password), length(password) == 1, !is.na(password), nzchar(password))
  sodium::password_store(password)
}

#' Определение является ли строка хешем пароля
#'
#' @param x вектор строк
#'
#' @returns логический вектор
is_password_hash <- function(x) {
  !is.na(x) & grepl("^\\$7\\$", x)
}

#' Проверка пароля
#'
#' @param password введённый пароль
#' @param stored значение из колонки password таблицы users
#'
#' @returns TRUE только если stored это хеш и пароль ему соответствует,
#'   пароли в открытом виде не сравниваются никогда
verify_password <- function(password, stored) {
  if (!is.character(password) || length(password) != 1 || is.na(password) || !nzchar(password)) return(FALSE)
  if (!is.character(stored)   || length(stored)   != 1 || !is_password_hash(stored)) return(FALSE)
  isTRUE(tryCatch(sodium::password_verify(stored, password), error = function(e) FALSE))
}

# хеш-заглушка, по ней проверяем пароль когда пользователь не найден,
# что бы время ответа не выдавало существование логина
.dummy_password_hash <- local({
  h <- NULL
  function() {
    if (is.null(h)) h <<- hash_password("dummy-password-for-timing")
    h
  }
})

#' Валидация нового пароля
#'
#' @param password пароль
#'
#' @returns NULL если пароль подходит, иначе текст ошибки
validate_new_password <- function(password) {
  if (!is.character(password) || length(password) != 1 || is.na(password) || !nzchar(password)) {
    return("Пароль не может быть пустым")
  }
  if (nchar(password) < PASSWORD_MIN_LENGTH) {
    return(paste0("Пароль должен содержать минимум ", PASSWORD_MIN_LENGTH, " символов"))
  }
  if (nchar(password) > PASSWORD_MAX_LENGTH) {
    return(paste0("Пароль должен содержать максимум ", PASSWORD_MAX_LENGTH, " символов"))
  }
  NULL
}

#' Проверка логина нового пользователя
#'
#' @param login логин
#'
#' @returns NULL если логин подходит, иначе текст ошибки
validate_new_login <- function(login) {
  if (!is.character(login) || length(login) != 1 || is.na(login) || !nzchar(trimws(login))) {
    return("Логин не может быть пустым")
  }
  if (nchar(login) > 64) return("Логин должен содержать максимум 64 символа")
  NULL
}

#' Установка пароля пользователю
#'
#' Записывает хеш пароля и отзывает все токены автовхода пользователя.
#'
#' @param conn соединение с app.db
#' @param login логин пользователя
#' @param password новый пароль в открытом виде
#'
#' @returns число изменённых пользователей (0 если логина нет)
set_user_password <- function(conn, login, password) {
  n <- DBI::dbExecute(
    conn,
    "UPDATE users SET password = ? WHERE login = ?",
    params = list(hash_password(password), login)
  )

  if (n > 0) {
    DBI::dbExecute(
      conn,
      "UPDATE auth_sessions SET revoked = 1 WHERE user_login = ?",
      params = list(login)
    )
  }

  n
}

#' Создание пользователя
#'
#' @param conn соединение с app.db
#' @param login логин
#' @param password пароль в открытом виде
#' @param role роль
#'
#' @returns TRUE если пользователь создан
create_user <- function(conn, login, password, role) {
  n <- DBI::dbExecute(
    conn,
    "INSERT INTO users (login, password, role) VALUES (?, ?, ?)",
    params = list(login, hash_password(password), role)
  )
  n == 1
}

#' Создание первого пользователя (администратора)
#'
#' Создаёт пользователя одним запросом и только если таблица users пустая,
#' поэтому из двух одновременных запросов сработает только один.
#'
#' @param conn соединение с app.db
#' @param login логин
#' @param password пароль в открытом виде
#'
#' @returns TRUE если пользователь создан, FALSE если пользователи уже есть
create_first_admin <- function(conn, login, password) {
  n <- DBI::dbExecute(
    conn,
    "INSERT INTO users (login, password, role)
     SELECT ?, ?, 'admin'
     WHERE NOT EXISTS (SELECT 1 FROM users)",
    params = list(login, hash_password(password))
  )
  n == 1
}

#' Проверка логина и пароля
#'
#' @param conn соединение с app.db
#' @param login логин
#' @param password пароль
#'
#' @returns data.frame из одной строки (login, role) или NULL
authenticate_user <- function(conn, login, password) {
  is_valid <- function(x) is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
  if (!is_valid(login) || !is_valid(password)) return(NULL)

  user <- DBI::dbGetQuery(
    conn,
    "SELECT login, role, password FROM users WHERE login = ?",
    params = list(login)
  )

  if (nrow(user) != 1) {
    # пользователя нет, но проверку делаем, что бы время ответа было одинаковым
    verify_password(password, .dummy_password_hash())
    return(NULL)
  }

  if (!verify_password(password, user$password[1])) return(NULL)

  user[, c("login", "role"), drop = FALSE]
}

#' Миграция паролей в открытом виде в хеши
#'
#' Перед изменениями делает резервную копию файла БД. Все изменения идут одной
#' транзакцией. Функцию можно запускать повторно: уже хешированные пароли
#' пропускаются. Сами пароли нигде не выводятся.
#'
#' @param db_path путь к файлу app.db
#' @param backup делать ли резервную копию
#'
#' @returns список: migrated (логины, у которых пароль захеширован), skipped (уже были хешами), backup (путь копии)
migrate_passwords_to_hash <- function(db_path, backup = TRUE) {
  stopifnot(file.exists(db_path))

  backup_path <- NA_character_
  if (backup) {
    backup_path <- paste0(db_path, ".bak_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    if (!file.copy(db_path, backup_path)) stop("Не удалось создать резервную копию: ", backup_path)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # ключ - login (уникален), колонки id в старых базах может не быть
  users <- DBI::dbGetQuery(con, "SELECT login, password FROM users")
  to_migrate <- users[!is_password_hash(users$password), , drop = FALSE]

  DBI::dbBegin(con)
  ok <- tryCatch({
    for (i in seq_len(nrow(to_migrate))) {
      plain <- to_migrate$password[i]
      if (is.na(plain) || !nzchar(plain)) stop("У пользователя ", to_migrate$login[i], " пустой пароль")

      new_hash <- hash_password(plain)
      # убеждаемся что хеш соответствует исходному паролю до записи
      if (!verify_password(plain, new_hash)) stop("Хеш не прошёл проверку для ", to_migrate$login[i])

      DBI::dbExecute(con, "UPDATE users SET password = ? WHERE login = ?",
                     params = list(new_hash, to_migrate$login[i]))
    }
    DBI::dbCommit(con)
    TRUE
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Миграция отменена, изменения не применены: ", conditionMessage(e), call. = FALSE)
  })

  list(
    migrated = to_migrate$login,
    skipped  = users$login[is_password_hash(users$password)],
    backup   = backup_path
  )
}
