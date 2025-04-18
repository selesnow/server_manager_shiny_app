## Описание
Пакет serviceaccounts предназначен для упрощённой работы с сервисными аккаунтами при авторизации в различных Google API, например Google Sheets API, или Google BigQuery API.


## Работа с пакетом
После установки в скриптах, в которых необходимо работать с Google Sheets API, изначально шерим доступ на почту `alsey-gsheets-readr@netpeak-1079.iam.gserviceaccount.com`, далее в скрипте используем одну из функций пакета для чтения данных сервисного аккаунта.


```r
library(googlesheets4)
library(bigrquery)

gs4_auth(path =  serviceaccounts::get_sa_from_env_var())
bq_auth(path = serviceaccounts::get_sa_from_env_var())
```

## Функции пакета

* `get_sa_from_internal_file()` - читает данные сервисного аккаунта непосредственно сохранённые в пакете.
* `get_sa_from_env_var()` - считывает путь к файлу сервисного аккаунта из переменной среды `SERVICE_GSHEETS_READR_ACCOUNT_PATH`.
* `get_sa_ls()` - выводит список доступных внутри пакета сервисных аккаунтов, по названию можно указывать любой в функции `get_sa_from_internal_file()`.

