library(git2r)
library(yaml)
library(dplyr)
library(tibble)
library(glue)

# Функция для получения содержимого blob через git show
get_blob_content <- function(repo_path, sha) {
  cmd <- glue('git -C "{repo_path}" show {sha}')
  system(cmd, intern = TRUE)
}

# Функция для получения списка измененных файлов в коммите
get_changed_files <- function(repo_path, commit_hash) {
  cmd <- glue('git -C "{repo_path}" show --name-only --pretty=format: {commit_hash}')
  files <- system(cmd, intern = TRUE)
  files[files != ""]
}

# Функция для получения содержимого файла в конкретном коммите
get_file_at_commit <- function(repo_path, commit_hash, file_path) {
  cmd <- glue('git -C "{repo_path}" show {commit_hash}:{file_path}')
  content <- tryCatch({
    system(cmd, intern = TRUE)
  }, error = function(e) {
    character(0)
  })
  return(content)
}

library(git2r)

get_all_commit_hashes <- function(repo_path) {
  # Открываем репозиторий
  repo <- repository(repo_path)
  
  # Получаем все коммиты
  commits <- commits(repo)
  
  # Извлекаем хеши
  hashes <- sapply(commits, function(commit) {
    git2r::sha(commit)
  })
  
  return(hashes)
}

library(git2r)
library(glue)

get_commits_version_history <- function(commit_hashes, repo_path) {
  # Результирующий датафрейм
  result <- data.frame(
    commit_hash = character(),
    commit_date = character(),
    author = character(),
    message = character(),
    version_changed = logical(),
    version = character(),
    stringsAsFactors = FALSE
  )
  
  # Открываем репозиторий
  repo <- repository(repo_path)
  
  # Функция для получения списка файлов в коммите
  get_files_in_commit <- function(hash) {
    cmd <- glue('git -C "{repo_path}" ls-tree -r --name-only {hash}')
    system(cmd, intern = TRUE)
  }
  
  # Функция для извлечения строки версии из векторного YAML
  extract_version_from_content <- function(content) {
    version_line <- grep("^version:", content, value = TRUE)
    if (length(version_line) > 0) {
      # Извлекаем строку версии, удаляя 'version:' и лишние символы
      version <- gsub("version:\\s*['\"]?([^'\"]+)['\"]?", "\\1", version_line)
      return(version)
    }
    return(NA_character_)
  }
  
  # Функция для получения содержимого файла в коммите
  get_file_content <- function(hash, file_path) {
    cmd <- glue('git -C "{repo_path}" show {hash}:{file_path}')
    tryCatch({
      system(cmd, intern = TRUE)
    }, error = function(e) {
      character(0)
    })
  }
  
  # Перебираем каждый хеш коммита
  for (hash in commit_hashes) {
    # Получаем информацию о коммите
    commit_obj <- tryCatch({
      lookup(repo, hash)
    }, error = function(e) {
      NULL
    })
    
    if (is.null(commit_obj)) {
      # Если не удалось получить коммит
      result <- rbind(result, data.frame(
        commit_hash = hash,
        commit_date = NA_character_,
        author = NA_character_,
        message = paste("Error: Could not find commit", hash),
        version_changed = FALSE,
        version = NA_character_,
        stringsAsFactors = FALSE
      ))
      next
    }
    
    # Получаем базовую информацию о коммите
    commit_date <- format(commit_obj$author$when)
    author_name <- commit_obj$author$name
    commit_message <- gsub("\n", " ", commit_obj$message)
    
    # Получаем список файлов в коммите
    files <- get_files_in_commit(hash)
    
    # Проверяем наличие файла app_info.yml
    version_changed <- "app_info.yml" %in% files
    app_version <- NA_character_
    
    # Если файл существует, получаем его содержимое
    if (version_changed) {
      content <- get_file_content(hash, "app_info.yml")
      if (length(content) > 1) {
        app_version <- extract_version_from_content(content)
      }
    }
    
    # Добавляем запись в результирующий датафрейм
    result <- rbind(result, data.frame(
      commit_hash = hash,
      commit_date = commit_date,
      author = author_name,
      message = commit_message,
      version_changed = version_changed,
      version = app_version,
      stringsAsFactors = FALSE
    ))
  }
  
  return(result)
}


commits_history <- get_commits_version_history(
  result$previous_commits,
  "C:/scripts/alsey/netpeak_core/nc_analytics_team/shiny_task_service_manager (dev)"
)

# Просмотр результатов
head(commits_history)
sum(commits_history$version_changed, na.rm = TRUE)  # Сколько коммитов содержат app_info.yml

#Пример использования:

all_com <- get_all_commit_hashes("C:/scripts/alsey/netpeak_core/nc_analytics_team/shiny_task_service_manager (dev)")
commits_history <- get_commits_version_history(
  all_com,
  "C:/scripts/alsey/netpeak_core/nc_analytics_team/shiny_task_service_manager (dev)"
)

write.csv(commits_history, file = 'commit_history.csv')

