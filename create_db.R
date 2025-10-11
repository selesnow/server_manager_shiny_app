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

# Вставка root-пользователя (если ещё не существует)
dbExecute(con, "
INSERT OR IGNORE INTO users (login, password, role)
VALUES ('root', 'root', 'admin');
")

# Вставка действий (если пусто)
dbExecute(con, "
INSERT OR IGNORE INTO actions (id, key, name, tab) VALUES
(1, 'AI Assistant Reset Chat', 'Сброс AI чата', 'AI Ассистент'),
(2, 'AI Assistant', 'Отправка сообщения в AI чат', 'AI Ассистент'),
(3, 'User add', 'Добавление нового пользователя', 'Доступы'),
(4, 'User remove', 'Удаление пользователя', 'Доступы'),
(5, 'User change role', 'Изменение роли пользователя', 'Доступы'),
(6, 'User password reset', 'Изменение пароля пользователя', 'Доступы'),
(7, 'CMD', 'Командная строка', 'CMD'),
(8, 'Find in files', 'Поиск по файлам', 'Поиск по файлам'),
(9, 'Process kill', 'Завершение процесса', 'Процессы'),
(10, 'Service start', 'Запуск службы', 'Службы'),
(11, 'Service stop', 'Остановка службы', 'Службы'),
(12, 'Service restart', 'Перезапуск службы', 'Службы'),
(13, 'Task run', 'Запуск задачи', 'Задачи'),
(14, 'Task run (popup)', 'Запуск задачи', 'Задачи'),
(15, 'Task log', 'Запрос лога задачи', 'Задачи'),
(16, 'Task script', 'Запрос кода задачи', 'Задачи'),
(17, 'Task README', 'Просмотрт README', 'Задачи'),
(18, 'Load task table to google sheet', 'Загрузка таблицы задач в Google Sheets', 'Задачи'),
(19, 'Rout ai analyze', 'Анализ Rout задачи', 'Задачи'),
(20, 'Script ai analyze', 'Анализ скрипта задачи', 'Задачи'),
(21, 'Task deactivate (popup)', 'Деактивация задачи', 'Задачи'),
(22, 'Task activate (popup)', 'Активация задачи', 'Задачи');
(23, 'Task deactivate', 'Деактивация задачи', 'Задачи');
(24, 'Task activate', 'Активация задачи', 'Задачи');
(25, 'Task NEWS', 'Просмотр NEWS', 'Задачи');
(26, 'Access config change', 'Изменение конфига доступов', 'Доступы');
(27, 'Logging config change', 'Изменение конфига логгирования', 'Доступы');
(28, 'Database config change', 'Изменение конфига баз данных', 'Доступы');
(29, 'Cleare app.Rout', 'Очистка app.Rout', 'Логи');
")

# Проверка
print(dbReadTable(con, "actions"))

# Закрыть соединение
dbDisconnect(con)
