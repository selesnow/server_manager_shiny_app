# Функция для создания индикатора к полям README, NEWS, Git, Rproj в информации о задачу
create_indicator <- function(field_name, field_value, display_name) {
  if (!is.null(field_value) && !is.na(field_value)) {
    if (field_value == TRUE) {
      span(
        display_name, " ", 
        span("✅", style = "color: green; font-size: 16px;"),
        style = "margin-right: 15px;"
      )
    } else {
      span(
        display_name, " ", 
        span("❌", style = "color: red; font-size: 16px;"),
        style = "margin-right: 15px;"
      )
    }
  } else {
    # Если поле отсутствует или NA, показываем серый индикатор
    span(
      display_name, " ", 
      span("❓", style = "color: gray; font-size: 16px;"),
      style = "margin-right: 15px;"
    )
  }
}
