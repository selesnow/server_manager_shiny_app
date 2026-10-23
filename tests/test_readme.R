# Тесты README: все вкладки и права описаны, картинки на месте, разметка корректна и
# превращается в HTML для вкладки README без встраивания картинок.
# Запуск из корня проекта: Rscript tests/test_readme.R

source("tests/helper.R")
source("create_config.R")
source("R/readme.R")

txt <- readLines("README.md", encoding = "UTF-8", warn = FALSE)
all_text <- paste(txt, collapse = "\n")
html <- as.character(readme_html("README.md"))
d <- default_config()

# ======================= содержание =======================
tabs <- c("Задачи", "Службы", "AI Ассистент", "Поиск по файлам", "Процессы", "CMD", "Доступ", "Статистика", "Логи", "README", "News")
chk("описана каждая вкладка приложения (заголовок раздела)", all(vapply(tabs, function(t) any(grepl(paste0("^### .*", t), txt)) || (t %in% c("README", "News") && any(grepl("^### .*README и News", txt))), logical(1))))
actions <- c("Запуск задач", "Активация задач", "Управление службами", "AI анализ", "Просмотр кода", "Управление мониторингом задач")
chk("права на действия из конфига по умолчанию упомянуты в README", all(actions %in% names(d$access_managemet)) && all(vapply(actions, function(a) grepl(a, all_text, fixed = TRUE), logical(1))))
chk("вкладки из конфига по умолчанию описаны (кроме служебных ключей Readme и News)", all(vapply(setdiff(names(d$access_managemet)[1:9], "Readme"), function(p) grepl(p, all_text, fixed = TRUE), logical(1))))

ai_src <- paste(readLines("modules/mod_tab_ai_assistant.R", encoding = "UTF-8", warn = FALSE), collapse = "\n")
cmds <- regmatches(ai_src, gregexpr("name = \"[a-z]+\"", ai_src, perl = TRUE))[[1]]
cmds <- sub("name = \"([a-z]+)\"", "\\1", cmds)
chk("команды чата из кода найдены и описаны в README", length(cmds) >= 3 && all(vapply(cmds, function(c) grepl(paste0("`/", c, "`"), all_text, fixed = TRUE), logical(1))))

tools_src <- paste(readLines("R/create_new_chat.R", encoding = "UTF-8", warn = FALSE), collapse = "\n")
chk("ассистент: права на действия в README совпадают с правами, которые проверяет код инструментов", all(vapply(c("Запуск задач", "Активация задач", "Управление мониторингом задач"), function(a) grepl(a, tools_src, fixed = TRUE) && grepl(a, all_text, fixed = TRUE), logical(1))))

# ======================= картинки =======================
imgs <- unique(unlist(regmatches(all_text, gregexpr("www/readme/[A-Za-z0-9_.-]+[.]png", all_text, perl = TRUE))))
chk("в README есть скриншоты", length(imgs) >= 5)
chk("все картинки из README лежат в www/readme и не пустые", all(file.exists(imgs)) && all(file.size(imgs) > 5000))
chk("в www/readme нет лишних файлов, на которые не ссылается README", setequal(file.path("www/readme", list.files("www/readme")), imgs))
chk("картинки не тяжёлые (каждая меньше 300 КБ)", all(file.size(imgs) < 300 * 1024))
chk("у каждой картинки есть alt-текст", all(grepl("alt=\"[^\"]+\"", grep("<img ", txt, value = TRUE))))

# ======================= разметка =======================
chk("блоки details открыты и закрыты парно", lengths(regmatches(all_text, gregexpr("<details>", all_text))) == lengths(regmatches(all_text, gregexpr("</details>", all_text))))
chk("у каждого details есть summary", lengths(regmatches(all_text, gregexpr("<details>", all_text))) == lengths(regmatches(all_text, gregexpr("<summary>", all_text))))
chk("в README нет длинных тире и пустых ячеек-заглушек", !grepl("—", all_text, fixed = TRUE))
chk("в README нет незакрытых блоков кода", sum(grepl("^```", txt)) %% 2 == 0)

# ======================= HTML для вкладки =======================
chk("README превращается в HTML: заголовки, таблицы и раскрывающиеся блоки на месте", grepl("<h1", html, fixed = TRUE) && grepl("<table>", html, fixed = TRUE) && grepl("<details>", html, fixed = TRUE))
chk("в HTML пути картинок сокращены до readme/ (приложение отдаёт www с корня)", !grepl("www/readme/", html, fixed = TRUE) && all(vapply(basename(imgs), function(f) grepl(paste0("src=\"readme/", f, "\""), html, fixed = TRUE), logical(1))))
chk("в HTML нет встроенных картинок (base64): страница не тяжелеет при каждом входе", !grepl("base64", html, fixed = TRUE))
chk("в HTML нет необработанной разметки markdown", !grepl("\\| ---", html) && !grepl("^#{1,3} ", html))

finish_tests()
