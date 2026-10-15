# Общий помощник для тестов в папке tests.
#
# Запуск любого теста из корня проекта:
#   Rscript tests/test_login_protection.R
#   Rscript tests/test_config.R
# Тесты работают с временными копиями баз и файлов, рабочие app.db и config.yaml
# не изменяются. Код завершения 0, если все проверки прошли, иначе 1.

.test_state <- new.env()
.test_state$fails  <- 0L
.test_state$checks <- 0L

#' Проверка с выводом результата
chk <- function(name, cond) {
  ok <- isTRUE(cond)
  .test_state$checks <- .test_state$checks + 1L
  if (!ok) .test_state$fails <- .test_state$fails + 1L
  cat(sprintf("%-80s %s\n", name, if (ok) "OK" else "!!! FAIL"))
  invisible(ok)
}

#' Итог тестов, при ошибках завершает процесс с кодом 1
finish_tests <- function() {
  cat(sprintf("\nПроверок: %d, ошибок: %d\n", .test_state$checks, .test_state$fails))
  if (.test_state$fails > 0L) quit(save = "no", status = 1L)
  invisible(TRUE)
}

#' Временный файл с автоматическим именем
temp_path <- function(name) file.path(tempdir(), name)
