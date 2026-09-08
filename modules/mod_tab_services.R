mod_tab_services_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Службы",
    # Скрипт для отслеживания двойного клика по строке таблицы служб
    tags$script(HTML(paste0("
    $(document).ready(function() {
      $(document).on('dblclick', '#", id, "-service_table tbody tr', function(e) {
        e.preventDefault();
        
        var serviceName = $(this).attr('data-service');
        
        if (!serviceName) {
          serviceName = $(this).find('td:eq(0)').text().trim(); // fallback на первую ячейку
        }
        
        console.log('Double-clicked on service row, serviceName:', serviceName);
        
        // Отправляем событие в Shiny
        Shiny.setInputValue('", id, "-service_table_cell_clicked', {
          serviceName: serviceName,
          value: 'dblclick',
          timestamp: new Date().getTime()
        }, {priority: 'event'});
      });
    });
    "))),
    
    # Блок с управлением службами
    fluidRow(
      column(
        width = 12,
        div(class = "card", 
            div(class = "card-header", "Управление службами"),
            div(class = "card-body",
                div(
                  h4("Выбор и управление службами"),
                  selectInput(ns("selected_service"), "Выберите службу:", choices = NULL),
                  textOutput(ns("service_status")),
                  div(class = "action-buttons-services",
                      uiOutput(ns("service_buttons"))
                  )
                )
            ),
            div(class = "mb-3", style = "margin-top: 5px; font-size: 0.9em; color: #bbb;",
                textOutput(ns("last_update"))
            )
        )
      )
    ),
    
    # Таблица служб
    fluidRow(
      column(
        width = 12,
        div(class = "card",
            div(class = "card-header", "Службы"),
            div(class = "card-body",
                DTOutput(ns("service_table"))
            )
        )
      )
    )
  )
}

mod_tab_services_server <- function(id, services_data, user_role, auth, session_id, conf_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    service_info <- reactive({
      req(input$selected_service)
      services_data() %>% filter(Service == input$selected_service)
    })
    
    output$service_table <- renderDT({
      df <- services_data() %>% select(-update_time)
      datatable(df,
                rownames = FALSE, # Отключаем нумерацию для прямого сопоставления data[0] -> Service
                filter   = "top",
                options  = list(
                  pageLength = 25, 
                  scrollX = TRUE,
                  rowCallback = DT::JS(
                    "function(row, data, index) {",
                    "  $(row).attr('data-service', data[0]);", # data[0] - имя службы
                    "}"
                  )
                ),
                selection = 'none') %>%
        formatStyle(columns = 1:ncol(df), cursor = 'pointer')
    })
    
    observeEvent(services_data(), {
      req(services_data())
      updateSelectInput(session, "selected_service", choices = services_data()$Service)
    })
    
    output$service_status <- renderText({
      current <- service_info()
      if (nrow(current) > 0) paste("Статус:", current$Status, "|", current$Description) else "Статус неизвестен"
    })
    
    output$service_buttons <- renderUI({
      role <- user_role()
      
      if (role %in% conf_rv()$access_managemet$`Управление службами`) {
        tagList(
          actionButton(ns("start_service"), "Запустить", icon = icon("play"), class = "btn-success"),
          actionButton(ns("stop_service"), "Остановить", icon = icon("stop"), class = "btn-danger"),
          actionButton(ns("restart_service"), "Перезапустить", icon = icon("sync"), class = "btn-warning")
        )
      }
    })
    
    # Время обновления данных
    output$last_update <- renderText({
      data <- services_data()
      req(data)
      
      last_time <- max(data$update_time, na.rm = TRUE)
      paste0("Данные обновлены: ", format(last_time, "%Y-%m-%d %H:%M:%S %Z"))
    })
    
    observeEvent(input$start_service, {
      write_action_log(user = auth$user()$login, func = 'Service start', session_id, value = input$selected_service)
      req(input$selected_service)
      system(glue::glue("nssm start {input$selected_service}"), intern = TRUE)
      showNotification("Служба запущена", type = "message")
    })
    
    observeEvent(input$stop_service, {
      write_action_log(user = auth$user()$login, func = 'Service stop', session_id, value = input$selected_service)
      req(input$selected_service)
      system(glue::glue("nssm stop {input$selected_service}"), intern = TRUE)
      showNotification("Служба остановлена", type = "warning")
    })
    
    observeEvent(input$restart_service, {
      write_action_log(user = auth$user()$login, func = 'Service restart', session_id, value = input$selected_service)
      req(input$selected_service)
      system(glue::glue("nssm restart {input$selected_service}"), intern = TRUE)
      showNotification("Служба перезапущена", type = "message")
    })
    
    # === Карточка службы и управление ей (двойной клик) ===
    current_modal_service <- reactiveVal(NULL)
    
    # Окно с подробной карточкой службы при двойном клике по строке таблицы
    observeEvent(input$service_table_cell_clicked, {
      click_info <- input$service_table_cell_clicked
      req(click_info)
      
      # Проверяем, что это двойной клик
      if (!is.null(click_info$value) && click_info$value == "dblclick") {
        service_name <- click_info$serviceName
        req(service_name)
        
        # Получаем данные службы
        current_data <- services_data() %>% filter(Service == service_name)
        req(nrow(current_data) > 0)
        
        row <- current_data[1, ]
        
        # Статус-бейдж
        status_badge <- if (row$Status == "Running" || grepl("RUNNING", row$Status, ignore.case = TRUE)) {
          span(row$Status, style = "background-color: rgba(16, 185, 129, 0.15); color: var(--success); padding: 4px 8px; border-radius: 6px; font-weight: 600; border: 1px solid rgba(16, 185, 129, 0.25);")
        } else {
          span(row$Status, style = "background-color: rgba(239, 68, 68, 0.15); color: var(--danger); padding: 4px 8px; border-radius: 6px; font-weight: 600; border: 1px solid rgba(239, 68, 68, 0.25);")
        }
        
        # Показываем модальное окно
        showModal(modalDialog(
          title = paste("Информация о службе:", row$Service),
          size = "m",
          tags$head(
            tags$style(HTML("
              .modal-content {
                  background-color: var(--bg-card) !important;
                  color: var(--text-primary) !important;
                  border: 1px solid var(--border-color) !important;
                  border-radius: 12px !important;
              }
              .modal-header, .modal-footer {
                  border: none !important;
              }
              .modal-title {
                  color: var(--text-primary) !important;
                  font-weight: bold;
              }
              .modal-body strong {
                  color: var(--primary) !important;
              }
            "))
          ),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Закрыть")
          ),
          
          div(
            div(class = "mb-2", strong("Название: "), span(row$Service)),
            div(class = "mb-2", strong("Отображаемое имя: "), span(row$DisplayName)),
            div(class = "mb-2", strong("Описание: "), span(row$Description)),
            div(class = "mb-2", strong("Статус: "), status_badge),
            div(class = "mb-2", strong("Клиент: "), span(row$Client)),
            div(class = "mb-2", strong("Директория: "), span(row$AppDirectory)),
            div(class = "mb-2", strong("Параметры: "), span(row$AppParameters)),
            div(class = "mb-2", strong("PID: "), span(row$PID)),
            div(class = "mb-2", strong("Время запуска: "), span(row$StartTime)),
            
            # Управление внутри модалки
            if (user_role() %in% conf_rv()$access_managemet$`Управление службами`) {
              tagList(
                tags$hr(),
                h4("Управление службой", style = "color: var(--primary); font-weight: 600; margin-bottom: 12px;"),
                div(
                  class = "d-flex gap-2",
                  actionButton(ns("modal_start_service"), "Запустить", icon = icon("play"), class = "btn btn-success"),
                  actionButton(ns("modal_stop_service"), "Остановить", icon = icon("stop"), class = "btn btn-danger"),
                  actionButton(ns("modal_restart_service"), "Перезапустить", icon = icon("sync"), class = "btn btn-warning")
                )
              )
            }
          )
        ))
        
        current_modal_service(service_name)
      }
    }, ignoreInit = TRUE)
    
    # --- Запуск из модалки ---
    observeEvent(input$modal_start_service, {
      req(current_modal_service())
      service_name <- current_modal_service()
      
      write_action_log(user = auth$user()$login, func = 'Service start', session_id, value = service_name)
      system(glue::glue("nssm start {service_name}"), intern = TRUE)
      showNotification("Служба запущена", type = "message")
      removeModal()
    })
    
    # --- Остановка из модалки ---
    observeEvent(input$modal_stop_service, {
      req(current_modal_service())
      service_name <- current_modal_service()
      
      write_action_log(user = auth$user()$login, func = 'Service stop', session_id, value = service_name)
      system(glue::glue("nssm stop {service_name}"), intern = TRUE)
      showNotification("Служба остановлена", type = "warning")
      removeModal()
    })
    
    # --- Перезапуск из модалки ---
    observeEvent(input$modal_restart_service, {
      req(current_modal_service())
      service_name <- current_modal_service()
      
      write_action_log(user = auth$user()$login, func = 'Service restart', session_id, value = service_name)
      system(glue::glue("nssm restart {service_name}"), intern = TRUE)
      showNotification("Служба перезапущена", type = "message")
      removeModal()
    })
  })
}
