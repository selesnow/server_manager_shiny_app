library(httr2)
library(jsonlite)

# Переменные окружения
gitlab_token <- Sys.getenv("GITLAB_PAT")
project_id <- URLencode(Sys.getenv("CI_PROJECT_ID"), reserved = TRUE)
pipeline_id <- Sys.getenv("CI_PIPELINE_ID")

tg_bot_token <- Sys.getenv("TG_BOT_TOKEN")  # Токен Telegram-бота
tg_chat_id <- Sys.getenv("TG_ERROR_CHAT_ID")  # ID чата для уведомлений

escape_markdown <- function(text) {
  # Список символов для экранирования
  special_chars <- c("_", "*")
  
  # Разделяем текст на части по бектикам
  parts <- unlist(strsplit(text, "`", fixed = TRUE))
  
  # Обрабатываем части: четные индексы - вне бектиков, нечетные - внутри
  for (i in seq_along(parts)) {
    # Если это часть вне бектиков (четный индекс при нумерации с 1)
    if (i %% 2 == 1) {
      # Экранируем специальные символы
      for (char in special_chars) {
        parts[i] <- gsub(paste0(char), paste0("\\", char), parts[i], fixed = TRUE)
      }
    }
    # Части внутри бектиков оставляем без изменений
  }
  
  # Собираем текст обратно, добавляя бектики между частями
  result <- parts[1]
  if (length(parts) > 1) {
    for (i in 2:length(parts)) {
      if (i %% 2 == 0) {
        # Добавляем бектик перед и после части внутри бектиков
        result <- paste0(result, "`", parts[i], "`")
      } else {
        # Просто добавляем часть вне бектиков
        result <- paste0(result, parts[i])
      }
    }
  }
  
  return(result)
}

# 1. Получаем все job из пайплайна
jobs_url <- paste0("https://git.netpeak.net/api/v4/projects/", project_id, "/pipelines/", pipeline_id, "/jobs")
jobs <- tryCatch({
  response <- request(jobs_url) |>
    req_headers("PRIVATE-TOKEN" = gitlab_token) |>
    req_perform()
  fromJSON(resp_body_string(response))
}, error = function(e) {
  message("Ошибка при получении списка jobs: ", conditionMessage(e))
  NULL
})

# 2. Фильтруем упавшие джобы
failed_jobs <- jobs[jobs$status == "failed", ]

if (nrow(failed_jobs) == 0) {
  message("Нет упавших jobs в данном пайплайне.")
} else {
  for (i in seq_len(nrow(failed_jobs))) {
    job_id <- failed_jobs[i, "id"]
    job_name <- failed_jobs[i, "name"]
    job_stage <- failed_jobs[i, "stage"]
    
    # 3. Получаем лог упавшей job
    logs_url <- paste0("https://git.netpeak.net/api/v4/projects/", project_id, "/jobs/", job_id, "/trace")
    logs <- tryCatch({
      response <- request(logs_url) |>
        req_headers("PRIVATE-TOKEN" = gitlab_token) |>
        req_perform()
      resp_body_string(response)
    }, error = function(e) {
      paste("Ошибка при получении логов: ", conditionMessage(e))
    })
    
    # 4. Берем 7 последних строк лога
    log_lines <- tail(strsplit(logs, "\n")[[1]], 14)
    log_excerpt <- paste(log_lines, collapse = "\n")
    
    # 5. Формируем сообщение для Telegram
    message_text <- paste(
      "🚨 *Ошибка в GitLab CI/CD* 🚨\n",
      "📌 *Проект:* netpeak\\_analytics/shiny\\_task\\_service\\_manager\n",
      "🔢 *Pipeline:* [", pipeline_id, "](https://git.netpeak.net/netpeak_analytics/shiny_task_service_manager/-/pipelines/", pipeline_id, ")\n",
      "💀 *Job:* [", escape_markdown(job_name), "](https://git.netpeak.net/netpeak_analytics/shiny_task_service_manager/-/jobs/", job_id, ") (stage:", escape_markdown(job_stage), ")\n",
      "❌ *ID Job:*", job_id, "\n",
      "📄 *Лог ошибки:*\n\n```", log_excerpt, "```\n",
      "🔗 [Перейти в GitLab](https://git.netpeak.net/netpeak_analytics/shiny_task_service_manager/-/jobs/", job_id, ")",
      sep = ""
    )
    
    # 6. Отправляем сообщение в Telegram
    tg_url <- paste0("https://api.telegram.org/bot", tg_bot_token, "/sendMessage")
    tg_payload <- list(
      chat_id = tg_chat_id,
      text = message_text,
      parse_mode = "Markdown"
    )
    tryCatch({
      request(tg_url) |>
        req_body_json(tg_payload) |>
        req_perform()
    }, error = function(e) {
      message("Ошибка при отправке в Telegram: ", conditionMessage(e))
    })
  }
}
