library(DBI)
library(RSQLite)

cat('Создание базы данных.')

# Путь к файлу базы
db_path <- conf$database_settings$app_data_base

# Подключение к базе (если файла нет — создастся)
con <- dbConnect(RSQLite::SQLite(), db_path)

# Создание таблиц
dbExecute(con, "
CREATE TABLE IF NOT EXISTS action_log (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  datetime   DATETIME,
  func_name  TEXT,
  user       TEXT,
  session_id TEXT,
  value      TEXT
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS session_log (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  datetime   DATETIME,
  session_id TEXT,
  user       TEXT,
  action     TEXT
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS error_log (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  datetime   DATETIME,
  session_id TEXT,
  user       TEXT,
  error      TEXT
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS ai_chat_log (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  datetime   DATETIME,
  session_id TEXT,
  user       TEXT,
  role       TEXT,
  message    TEXT
);
")

dbExecute(con, "
-- find_in_files_log definition

CREATE TABLE find_in_files_log (
    search_id   INTEGER,
    session_id  TEXT,
    user        TEXT,
    query       TEXT,
    file        TEXT,
    line        TEXT,
    match       TEXT,
    datetime    TEXT, 
    extensions TEXT, 
    \"task name\" TEXT, 
    \"task state\" TEXT
    );
")

dbExecute(con, "
-- cmd_log definition

CREATE TABLE cmd_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    datetime   TEXT, 
    session_id TEXT,
    user       TEXT,
    cmd        TEXT,
    result     TEXT
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

dbExecute(con, "
CREATE TABLE IF NOT EXISTS actions (
  id    INTEGER PRIMARY KEY AUTOINCREMENT,
  key   TEXT NOT NULL,
  name  TEXT NOT NULL,
  tab   TEXT NOT NULL
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS auth_sessions (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_login TEXT NOT NULL,
  token_hash TEXT NOT NULL,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  expires_at DATETIME NOT NULL,
  revoked INTEGER DEFAULT 0
);
")

# Пользователей по умолчанию не создаём: при первом запуске приложение
# предложит создать первого пользователя с ролью admin.

# Заполнение справочника действий, список лежит в R/actions.R
source('R/actions.R')
sync_actions(con)

# Проверка
print(dbReadTable(con, "actions"))

# Закрыть соединение
dbDisconnect(con)
