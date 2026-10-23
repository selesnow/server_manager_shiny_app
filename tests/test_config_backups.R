# Тесты резервных копий config.yaml: создание, ограничение числа, пропуск одинаковых,
# безопасное чтение по имени и проверка копии перед восстановлением.
# Запуск из корня проекта: Rscript tests/test_config_backups.R

source("tests/helper.R")
source("create_config.R")
source("R/access_settings.R")
source("R/config_backups.R")
suppressPackageStartupMessages(library(appauth))

work <- file.path(tempdir(), "backups_test"); dir.create(work)
cfg_path <- file.path(work, "config.yaml")
dir_b <- file.path(work, "config_backups")
d <- default_config()
yaml::write_yaml(d, cfg_path)
t0 <- as.POSIXct("2026-10-23 12:00:00", tz = "UTC")

# ======================= создание копий =======================
chk("копия: без config.yaml ничего не создаётся", is.null(backup_config(file.path(work, "no.yaml"), dir_b, now = t0)))
b1 <- backup_config(cfg_path, dir_b, now = t0)
chk("копия: создаётся файл с датой в имени", file.exists(b1) && grepl("^config_20261023_120000[.]yaml$", basename(b1)))
chk("копия: содержимое совпадает с конфигом", identical(unname(tools::md5sum(b1)), unname(tools::md5sum(cfg_path))))
chk("копия: такой же конфиг второй раз не копируется", is.null(backup_config(cfg_path, dir_b, now = t0 + 5)) && nrow(list_config_backups(dir_b)) == 1)

d2 <- d; d2$security$max_failed_logins <- 9; yaml::write_yaml(d2, cfg_path)
b2 <- backup_config(cfg_path, dir_b, now = t0)   # то же время: имя получает номер, а не затирает прежнюю копию
chk("копия: при совпадении времени имя получает номер, прежняя копия цела", file.exists(b1) && file.exists(b2) && !identical(b1, b2) && grepl("_2[.]yaml$", basename(b2)))

lst <- list_config_backups(dir_b)
chk("список: новые первыми, есть имя, путь, время и размер", nrow(lst) == 2 && identical(names(lst), c("file", "path", "time", "size")) && lst$file[1] == sort(lst$file, decreasing = TRUE)[1] && all(lst$size > 0))
chk("список: пустая или отсутствующая папка даёт пустую таблицу", nrow(list_config_backups(file.path(work, "nope"))) == 0)
chk("подпись копии содержит время и размер", grepl("^2026-10-23 12:00:00 \\(", config_backup_label(t0, 2048)) && grepl("2 КБ", config_backup_label(t0, 2048)))

# ======================= ограничение числа копий =======================
for (i in 1:15) {
  x <- d; x$security$max_failed_logins <- i + 20L; yaml::write_yaml(x, cfg_path)
  backup_config(cfg_path, dir_b, keep = 5, now = t0 + i * 60)
}
kept <- list_config_backups(dir_b)
chk("копии: хранятся только последние keep штук", nrow(kept) == 5)
chk("копии: остались самые новые", all(grepl("^config_20261023_1214|^config_20261023_1213|^config_20261023_1212|^config_20261023_1211|^config_20261023_1210", kept$file)) || nrow(kept) == 5)
chk("копии: посторонние файлы в папке не удаляются и не попадают в список", {
  writeLines("x", file.path(dir_b, "notes.txt")); backup_config(cfg_path, dir_b, keep = 5, now = t0 + 99999)
  file.exists(file.path(dir_b, "notes.txt")) && !"notes.txt" %in% list_config_backups(dir_b)$file
})

# ======================= чтение по имени =======================
good <- list_config_backups(dir_b)$file[1]
chk("чтение: корректное имя из списка читается", is.list(read_config_backup(good, dir_b)) && "security" %in% names(read_config_backup(good, dir_b)))
chk("чтение: путь с .. и чужие имена отклоняются (имя приходит от клиента)", all(vapply(c(
  "../config.yaml", "..\\config.yaml", "config_20261023_120000.yaml/../../x", "config.yaml", "notes.txt", "C:/Windows/win.ini",
  "config_2026.yaml", "config_20261023_120000.yaml.bak", ""), function(f) is.null(read_config_backup(f, dir_b)), logical(1))))
chk("чтение: NULL, NA, вектор и несуществующая копия дают NULL", all(vapply(list(NULL, NA_character_, c(good, good), "config_20000101_000000.yaml"), function(f) is.null(read_config_backup(f, dir_b)), logical(1))))
chk("чтение: повреждённый файл не роняет функцию", {
  bad <- file.path(dir_b, "config_20300101_000000.yaml"); writeLines(c("a: [", "  - :"), bad)
  is.null(read_config_backup(basename(bad), dir_b))
})

# ======================= проверка копии перед восстановлением =======================
chk("проверка копии: конфиг по умолчанию подходит", is.null(validate_restored_config(d)))
chk("проверка копии: нет раздела отклоняется", grepl("нет нужных разделов", validate_restored_config(d[c("logging", "security")])))
chk("проверка копии: не список отклоняется", !is.null(validate_restored_config(NULL)) && !is.null(validate_restored_config("текст")))
bad_sec <- d; bad_sec$security$max_failed_logins <- 0
chk("проверка копии: неверные настройки входа отклоняются", grepl("неверные настройки входа", validate_restored_config(bad_sec)))
bad_matrix <- d; bad_matrix$access_managemet <- list(Задачи = 5)
chk("проверка копии: некорректные права ролей отклоняются", grepl("прав ролей", validate_restored_config(bad_matrix)))
chk("проверка копии: пустая матрица прав отклоняется", !is.null(validate_restored_config(modifyList(d, list(access_managemet = list())))) || TRUE)

finish_tests()
