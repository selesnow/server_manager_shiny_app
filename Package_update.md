В проект добавлен renv, в связи с чем процесс обновления пакетов в проекте идёт следующим образом:

1. Обновление пакета

```r
renv::install("shiny@1.9.0") 
```

2. Обновляешь lock:

```r
renv::snapshot()
```

3. Тестируешь.
4. Коммит → push в master.


На сервере (prod):

GitLab Runner автоматически выполнит:

1. stop_app
2. install renv
3. git pull (получит новый dev/renv.lock)
4. renv::restore() — поставит новые версии пакетов в prod
5. start_app
6. notify