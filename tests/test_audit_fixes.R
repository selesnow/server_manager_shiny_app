# Тесты исправлений аудита безопасности (релиз 1.43.0): экранирование таблицы логов,
# копирование названия задачи без JS-кода, проверка пути лога, закрытие подключения.
# Запуск из корня проекта: Rscript tests/test_audit_fixes.R

source("tests/helper.R")
source("R/safe_ui.R")
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

# ======================= A1. Таблица логов AI-чата =======================
df <- data.frame(id = 1, datetime = "2026-10-23", session_id = "s", user = "<img src=x onerror=alert(1)>",
                 role = "user", message = "**текст**", stringsAsFactors = FALSE)
esc <- ai_log_escape_columns(df)
chk("escape: колонка message не экранируется, остальные экранируются", identical(esc, -6L) || identical(esc, -match("message", names(df))))
chk("escape: без колонки message экранируются все", isTRUE(ai_log_escape_columns(data.frame(user = "a"))))
html <- ai_log_message_html(c("**жирный**", "<script>alert(1)</script>", "[ссылка](javascript:alert(1))",
                               "<img src=x onerror=alert(1)>", "[ок](https://example.com)", "> цитата", "[данные](data:text/html;base64,AAAA)"))
chk("markdown: жирный текст рендерится", grepl("<strong>жирный</strong>", html[1], fixed = TRUE))
chk("markdown: сырой <script> выводится текстом, тега нет", !grepl("<script", html[2], ignore.case = TRUE) && grepl("&lt;script", html[2], fixed = TRUE))
chk("markdown: javascript:-ссылка заменена", !grepl("javascript:", html[3], ignore.case = TRUE))
chk("markdown: сырой <img onerror> не становится тегом", !grepl("<img", html[4], ignore.case = TRUE))
chk("markdown: обычная https-ссылка сохраняется", grepl("href=\"https://example.com\"", html[5], fixed = TRUE))
chk("markdown: заменённая ссылка получила href=\"#\" без служебных символов", grepl("href=\"#\"", html[3], fixed = TRUE) && !grepl("[[:cntrl:]]", sub("\n$", "", html[3])))
chk("markdown: цитата по-прежнему рендерится", grepl("<blockquote>", html[6], fixed = TRUE))
chk("markdown: data:-ссылка заменена", !grepl("data:", html[7], fixed = TRUE))
src_log <- readLines("modules/mod_tab_log.R", encoding = "UTF-8", warn = FALSE)
src_log <- src_log[!grepl("^[[:space:]]*#", src_log)]
chk("код вкладки логов использует escape по колонкам, а не escape = FALSE", !any(grepl("escape[[:space:]]*=[[:space:]]*FALSE", src_log)))

# ======================= A2. Копирование названия задачи =======================
msgs <- list()
fake <- list(sendCustomMessage = function(type, message) msgs[[length(msgs) + 1]] <<- list(type = type, message = message))
bad <- "task'); alert(document.cookie); ('"
chk("clipboard_copy: название уходит данными без изменений", {
  clipboard_copy(fake, bad)
  length(msgs) == 1 && msgs[[1]]$type == "copyToClipboard" && identical(msgs[[1]]$message, bad)
})
chk("clipboard_copy: не строка и NA игнорируются", {
  n <- length(msgs)
  clipboard_copy(fake, NULL); clipboard_copy(fake, c("a", "b")); clipboard_copy(fake, NA_character_); clipboard_copy(fake, 1)
  length(msgs) == n
})
src_tasks <- readLines("modules/mod_tab_tasks.R", encoding = "UTF-8", warn = FALSE)
chk("в модуле задач нет runjs с подстановкой значений в JS-код (только постоянные строки)",
    !any(grepl("writeText", src_tasks, fixed = TRUE)) && !any(grepl("runjs[(](sprintf|paste|glue|str_c|js_code)", src_tasks)))

# ======================= A3. Файл лога от клиента =======================
allowed <- c(a = "C:/scripts/x/log1.Rout", b = "C:/scripts/x/log2.Rout")
chk("файл из списка задачи разрешён", log_file_allowed("C:/scripts/x/log1.Rout", allowed))
chk("файл вне списка отклоняется", !log_file_allowed("C:/Windows/System32/config/SAM", allowed))
chk("путь с .. отклоняется", !log_file_allowed("C:/scripts/x/../../Users/a/.Renviron", allowed))
chk("не строка и NA отклоняются", !log_file_allowed(NULL, allowed) && !log_file_allowed(NA_character_, allowed) && !log_file_allowed(c(allowed[[1]], allowed[[2]]), allowed))
chk("пустой список разрешённых отклоняет всё", !log_file_allowed("C:/scripts/x/log1.Rout", character(0)))
chk("модуль задач использует log_file_allowed перед чтением", any(grepl("log_file_allowed(input$selected_log_file", src_tasks, fixed = TRUE)))

# ======================= A4. Подключение в get_tasks =======================
src_gt <- readLines("R/get_tasks.R", encoding = "UTF-8", warn = FALSE)
i_con <- grep("dbConnect(", src_gt, fixed = TRUE)[1]
chk("get_tasks: сразу после dbConnect стоит on.exit с dbDisconnect", grepl("on.exit(dbDisconnect(con)", src_gt[i_con + 1], fixed = TRUE))
f <- function(path) { con <- dbConnect(SQLite(), path); on.exit(dbDisconnect(con), add = TRUE); con }
tmp <- tempfile(fileext = ".db"); con <- f(tmp)
chk("шаблон on.exit закрывает подключение после выхода из функции", !dbIsValid(con))

finish_tests()
