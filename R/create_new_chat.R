# Функция для создания нового чата (выносим в отдельную функцию)
create_new_chat <- function(user_role, conf_temp) {
  
  message("Роль пользователя: ", user_role)

  new_chat <- ellmer::chat_google_gemini(
    system_prompt = interpolate_file(path = here::here('ai_docs', 'system_prompt.md')),
    model = 'gemini-2.0-flash',  
    echo  = 'none'
  )
  
  # добавление ассистенту инструментов
  new_chat$register_tool(tool(
    Sys.Date,
    name = "get_current_date",
    description = "Получить сегодняшнюю дату, поможет при запросе данных за текущий спринт, или запросе расчётов по юнит экономике за текущий месяц"
  ))
  
  new_chat$register_tool(tool(
    get_task_log,
    name = "get_task_log",
    description = "Получить текст лога выполнения скрипта по названию задачи из планировщика заданий Windows",
    arguments = list(
      task_name = type_string(
        "Название задачи из планировщика заданий Windows по которой надо получить лог выполнения (Rout файл) запускаемого скрипта"
      )
    )
  ))
  
  new_chat$register_tool(tool(
    get_task_script,
    name = "get_task_script",
    description = "Получить листинг скрипта запускаемого определённой задачей из планировщика заданий Windows по названию задачи",
    arguments = list(
      task_name = type_string(
        "Название задачи из планировщика заданий Windows по которой надо получить лог выполнения (Rout файл) запускаемого скрипта"
      )
    )
  ))
  
  new_chat$register_tool(tool(
    get_task_info,
    name = "get_task_info",
    description = "Получить информацию об определённой задачей из планировщика заданий Windows по её названию.",
    arguments = list(
      task_name = type_string(
        "Название задачи из планировщика заданий Windows по которой надо получить лог выполнения (Rout файл) запускаемого скрипта"
      )
    )
  ))
  
  new_chat$register_tool(tool(
    get_failed_tasks,
    name = "get_failed_tasks",
    description = "Получить информацию о задачах из планировщика Windows, работа которых при прошлом запуске завершилась ошибкой. Функция вернёт название задач и код ошибки из планировщика."
  ))
  
  new_chat$register_tool(tool(
    get_tasks_csv,
    name = "get_tasks_csv",
    description = "Получить CSV таблицу с данными по задачам из панировщика заданий Windows. С их описанием и всеми параметрами. Эту функцию удобно использовать для поиска нужных задач по запросу пользователя."
  ))
  
  new_chat$register_tool(tool(
    find_pf_task_data,
    name = "find_pf_task_data",
    description = "Получить данные о задаче из Планфикс, описание задачи хранится в поле description. Данные возвращаются в JSON формате.",
    arguments = list(
      task_link = type_string(
        "Ссылка на задачу в Планфикс."
      )
    )
  ))
  
  new_chat$register_tool(tool(
    get_sprint_planfix_tasks,
    name = "get_sprint_planfix_tasks",
    description = "Функция которая позволяет запросить задачи из Планфик (не из планировщика заданий Windows!) команды аналитиков, для анализа их спринта и успешности выполнения ими задач. По умолчанию возвращаются данные за текущий спринт.",
    arguments = list(
      year = type_string("Год в формате двухзначного числа (format(Sys.Date(), '%y')) если 2025 год то 25, если 2024 то 24, и так далее, по умолчанию берётся текущий год."),
      month = type_string("Месяц в формате двухзначного числа (format(Sys.Date(), '%m')) если январь то 01, если май то 05, и так далее, по умолчанию берётся текущий месяц.")
    )
  ))
  
  new_chat$register_tool(tool(
    unit_economics_analytics,
    name = "unit_economics_analytics",
    description = "Функция для получения данных для расчёта дохода по Юнит Экономике. Каждая строка это отдельная задача, по которой мы заработали деньги (fact_cost). И на которую потратили определённое время (fact_hours).",
    arguments = list(
      date_from = type_string("Начальная дата периода расчёта в формате YYYY-MM-DD."),
      date_to = type_string("Конечная дата периода расчёта в формате YYYY-MM-DD.")
    )
  ))
  
  # Тут мы проверяем может ли текущий пользователь со своей ролью управлять задачами
  # если его роль не позволяет это делать то отправляем уведомление модели о том что у пользователя недостаточно прав
  if (user_role %in% conf_temp$access_managemet$`Запуск задач`) {
    task_management <- 'enable'
  } else {
    task_management <- 'disable'
  }
    
  new_chat$register_tool(tool(
    run_server_task_ls[[task_management]],
    name = "run_server_task",
    description = "Запуск задачи в планировщике заданий Windows на сервере. Важно при передаче аргумента task_name не добавлять в назании задачь лищние слешы.",
    arguments = list(
      task_name = type_string(
        "Название задачи из планировщика заданий Windows которую надо запустить на сервере."
      )
    )
  ))
    
  if (user_role %in% conf_temp$access_managemet$`Активация задач`) {
    task_management <- 'enable'
  } else {
    task_management <- 'disable'
  }
  
  new_chat$register_tool(tool(
    task_state_change_ls[[task_management]],
    name = "task_state_change",
    description = "Активировать (включать) и деактивировать (выключать) залачи в планировщике заданий Windows, изменяет параметр Scheduled Task State.",
    arguments = list(
      task_name = type_string(
        "Название задачи из планировщика заданий Windows по которой надо получить лог выполнения (Rout файл) запускаемого скрипта"
      ),
      action = type_string(
        "Какое действие надо выполнить с задачей, Enable - активировать (включить) задачу, Disable - деактивировать (отключить) заадчу."
      )
    )
  ))
  
  return(new_chat)
}
