
## Работа с пакетом segments

Пакет предназначен для обогащения дополнительными данными информации о списаниях и проектах, которую вы можете получить с помощью пакета `rpup`.

## Функции пакета segments

Функции пакета можно разбить на 2 группы:

### Функции обогощение данных

Это семейство функций получает на вход таблицу со списаниями или проектами, и добалвяет в них дополнительные столбцы:

* `detect_segments()` - Используется для определения сегмента по дате, сфере и PM по списаниям или просто проектам. Добавляет поле segment.
* `detect_region_group()` - Используется для определения региона по представительству по списаниям или просто проектам. Добавляет поле region.
* `detect_sphere_group()` - Используется для определения группы сфере по сфере по списаниям или просто проектам. Добавляет поле sphere_group.
* `detect_company()` - Используется для определения компании по сегменту, группе сфер и региону по списаниям или просто проектам. Добавлет поле company.
* `detect_sales_segments` - Используется для определения сегмента по дате первой оплаты, департаменту и сфере в реестре продаж. Добавлет поле segment.
* `detect_sales_company()` - Используется для определения компании по сегменту, группе сфер и региону в таблице реестре продаж. Добавлет поле company.
* `detect_language()` - Используется для определения поля region_language ("ua-ru", "eng-bg"), по группе регионов и языку коммуникаций.

### Функции чтения справочников

Это семейство функций обычно не используются конечными пользователями пакета напрямую, т.к. они являются вспомогательными функциями, и используются внутри функций обогащения данных.

* `read_segments_dictionary()` - Чтение справочника сегментов для списаний или проектов.
* `read_regional_dictionary()` - Чтение справочника регионов для списаний или проектов.
* `read_sphere_group_dictionary()` - Чтение справочника грумм сфер для списаний или проектов.
* `read_company_dictionary()` - Чтение справочника компаний для списаний или проектов.
* `read_languages_dictionary()` - Чтение справочника язык-регион для списаний или проектов. 
* `read_sales_segments_dictionary()` - Чтение справочника сегментов для реестра продаж.
* `read_sales_company_dictionary()` - Чтение справочника компаний для реестра продаж.

Все функции чтения справочников под капотом обращаются к справочникам, которые хранятся в этом [доксе]( https://docs.google.com/spreadsheets/d/13I-8yA0wH7OOlQzSdKPMgeHi5gW4MNANrrK55dX2YXY/), и там же администрируются по необходимости. За актуальность справочников отвечает [Ida](https://t.me/ida_np).

## Пример работы с пакетом segments и таблицей списаний

```r
library(rpup)
library(segments)
library(googlesheets4)

pup_connection()

# загрузка списаний
writeoffs <- pup_get_table(
  'payment_report',
  filters = 'date >= "2020-01-01"',
  fields = c(
    'payment_id', 
    'date', 
    'pm', 
    'service_sphere',
    'agency',
    'fsr'
  )
)

pup_disconnect()

# обогощяем таблицу справочными данными
writeoffs <- writeoffs %>% 
             detect_segments() %>%      # Добавляем поле segment
             detect_region_group() %>%  # Добавляем поле region
             detect_sphere_group() %>%  # Добавляем поле sphere_group
             detect_company()           # Определение компании
             
# Чтение реестра продаж
sales <- range_read('1iqCwYiJmAemPsatRlz_VlZgLtFPKwGKqGZLaryZCpc8', sheet = 'BD only') %>% 
         detect_sales_segments() # Добавляем поле segment
         
# Для добавление поля region_language нам необходимо запросить язык коммуникации по клиентам
clients <- pup_get_clients(
  with_archive = T,
  fields = c('client_id', 'communication_language')
)

# Запрашиваем списания
writeoffs <- pup_get_table('payment_report', filters = 'date >= "2021-01-01"') %>% 
             left_join(clients) # дополняем данные списаний информацией о клиенте

# Определяем регион и язык-регион
writeoffs <- writeoffs %>% 
  detect_region_group() %>% # определяем регион
  detect_language() # определяем поле region_language

```

## Пример работы с реестром продаж

Реестр продажхранится в [доксе](https://docs.google.com/spreadsheets/d/1iqCwYiJmAemPsatRlz_VlZgLtFPKwGKqGZLaryZCpc8/edit?gid=1744098474#gid=1744098474), из года в год этот докс может менятся, ведёт его в ручном режиме отдел продаж.

```r
library(segments)
library(googlesheets4)

gs4_auth('a.seleznev@netpeak.group')

sales <- range_read(
  ss = '1iqCwYiJmAemPsatRlz_VlZgLtFPKwGKqGZLaryZCpc8', 
  sheet = 'BD only'
  ) %>% 
  detect_sales_segments() %>% 
  detect_region_group(
    agency_col_name = 'Region from PUP', 
    region_col = 'region_sales', 
    dict_lang = 'ru'
  ) %>% 
  detect_sphere_group(sphere_col_name = 'Area') %>% 
  detect_sales_company()
```

## Поддержка пакета

По всем вопросам работы с пакетом, и расширения его функционала
обращаться к [Alsey](https://t.me/AlexeySeleznev).
