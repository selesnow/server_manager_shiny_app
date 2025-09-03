mod_config_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Настройки",
    fluidRow(
      column(
        6,
        h4("Редактирование ролей"),
        uiOutput(ns("role_editor")),
        actionButton(ns("save_config"), "💾 Сохранить изменения", class = "btn-success")
      )
    )
  )
}

mod_config_server <- function(id, conf_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Реактивный YAML-конфиг (передаём в модуль)
    conf <- conf_rv
    
    # 1. Отображаем дерево
    output$config_tree <- shinyTree::renderTree({
      conf()
    })
    
    # 2. Отображаем JSON
    output$config_json <- listviewer::renderJsonedit({
      conf()
    })
    
    # 3. UI для редактирования tab_* узлов
    output$role_editor <- renderUI({
      conf_list <- conf()
      tab_nodes <- conf_list$access_managemet
      
      role_choices <- c("admin", "user", "viewer")
      
      lapply(names(tab_nodes), function(node) {
        selectInput(
          ns(paste0("roles_", node)),
          label = node,
          choices = role_choices,
          selected = tab_nodes[[node]],
          multiple = TRUE
        )
      })
    })
    
    # 4. Обновляем конфиг при сохранении
    observeEvent(input$save_config, {
      new_conf <- conf()
      
      tab_nodes <- names(new_conf$access_managemet)
      for (node in tab_nodes) {
        input_id <- paste0("roles_", node)
        if (!is.null(input[[input_id]])) {
          new_conf$access_managemet[[node]] <- input[[input_id]]
        }
      }
      
      # Сохраняем в реактивное хранилище
      conf(new_conf)
      
      # Перезаписываем YAML файл
      yaml::write_yaml(new_conf, "config.yaml")
      
      showNotification("✅ Конфигурация обновлена", type = "message")
    })
  })
}
