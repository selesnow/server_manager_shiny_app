#' Справочник действий пользователей для логов (таблица actions)
#'
#' Ключ key должен совпадать со значением аргумента func в вызовах
#' write_action_log(). При добавлении нового действия в приложение добавьте
#' строку с новым id в конец списка, при следующем запуске приложения она
#' появится в базе (см. sync_actions).
actions_catalog <- function() {
  tibble::tribble(
    ~id, ~key,                              ~name,                                     ~tab,
    1,   'AI Assistant Reset Chat',         'Сброс AI чата',                           'AI Ассистент',
    2,   'AI Assistant',                    'Отправка сообщения в AI чат',             'AI Ассистент',
    3,   'User add',                        'Добавление нового пользователя',          'Доступы',
    4,   'User remove',                     'Удаление пользователя',                   'Доступы',
    5,   'User change role',                'Изменение роли пользователя',             'Доступы',
    6,   'User password reset',             'Изменение пароля пользователя',           'Доступы',
    7,   'CMD',                             'Командная строка',                        'CMD',
    8,   'Find in files',                   'Поиск по файлам',                         'Поиск по файлам',
    9,   'Process kill',                    'Завершение процесса',                     'Процессы',
    10,  'Service start',                   'Запуск службы',                           'Службы',
    11,  'Service stop',                    'Остановка службы',                        'Службы',
    12,  'Service restart',                 'Перезапуск службы',                       'Службы',
    13,  'Task run',                        'Запуск задачи',                           'Задачи',
    14,  'Task run (popup)',                'Запуск задачи',                           'Задачи',
    15,  'Task log',                        'Запрос лога задачи',                      'Задачи',
    16,  'Task script',                     'Запрос кода задачи',                      'Задачи',
    17,  'Task README',                     'Просмотрт README',                        'Задачи',
    18,  'Load task table to google sheet', 'Загрузка таблицы задач в Google Sheets',  'Задачи',
    19,  'Rout ai analyze',                 'Анализ Rout задачи',                      'Задачи',
    20,  'Script ai analyze',               'Анализ скрипта задачи',                   'Задачи',
    21,  'Task deactivate (popup)',         'Деактивация задачи',                      'Задачи',
    22,  'Task activate (popup)',           'Активация задачи',                        'Задачи',
    23,  'Task deactivate',                 'Деактивация задачи',                      'Задачи',
    24,  'Task activate',                   'Активация задачи',                        'Задачи',
    25,  'Task NEWS',                       'Просмотр NEWS',                           'Задачи',
    26,  'Access config change',            'Изменение конфига доступов',              'Доступы',
    27,  'Logging config change',           'Изменение конфига логгирования',          'Доступы',
    28,  'Database config change',          'Изменение конфига баз данных',            'Доступы',
    29,  'Cleare app.Rout',                 'Очистка app.Rout',                        'Логи',
    # ключи, которые реально пишет код (раньше в справочнике были другие названия)
    30,  'Activate task',                   'Активация задачи',                        'Задачи',
    31,  'Deactivate task',                 'Деактивация задачи',                      'Задачи',
    32,  'AI Assistant Clear Chat Button',  'Очистка AI чата кнопкой',                 'AI Ассистент'
  )
}

#' Добавление в таблицу actions недостающих действий
#'
#' Существующие строки не изменяются (сопоставление по id), функцию можно
#' вызывать при каждом запуске приложения.
#'
#' @param conn соединение с app.db
#'
#' @returns число добавленных действий
sync_actions <- function(conn) {
  catalog <- actions_catalog()

  added <- 0
  for (i in seq_len(nrow(catalog))) {
    added <- added + DBI::dbExecute(
      conn,
      "INSERT OR IGNORE INTO actions (id, key, name, tab) VALUES (?, ?, ?, ?)",
      params = list(catalog$id[i], catalog$key[i], catalog$name[i], catalog$tab[i])
    )
  }

  added
}
