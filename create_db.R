# Скрипт разворачивает базу данных приложения

library(DBI)
library(RSQLite)

# Путь к файлу базы
db_path <- "app.db"

# Подключение к базе (если файла нет — создастся)
con <- dbConnect(RSQLite::SQLite(), db_path)

# Создание таблиц
dbExecute(con, "
CREATE TABLE IF NOT EXISTS action_log (
  id         INTEGER PRIMARY KEY AUTOINCREMENT,
  datetime   DATETIME,
  func_name  TEXT,
  user       TEXT,
  session_id TEXT
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS session_log (
  id         INTEGER PRIMARY KEY AUTOINCREMENT,
  datetime   DATETIME,
  session_id TEXT,
  user       TEXT,
  action     TEXT
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS users (
  id       INTEGER PRIMARY KEY AUTOINCREMENT,
  login    TEXT UNIQUE NOT NULL,
  password TEXT NOT NULL,
  role     TEXT NOT NULL
);
")

# Вставка root-пользователя (если ещё не существует)
dbExecute(con, "
INSERT OR IGNORE INTO users (login, password, role)
VALUES ('root', 'root', 'admin');
")

# Проверка
print(dbReadTable(con, "users"))

# Закрыть соединение
dbDisconnect(con)
