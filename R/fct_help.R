# Small, reusable UI helpers for offline CO2 guidance.
#
# The help is deliberately rendered as native HTML details elements so it works
# inside the sandbox without JavaScript or an internet connection.
.co2_help <- function(title = "Show help", ...) {
  htmltools::tags$details(
    class = "co2-inline-help",
    htmltools::tags$summary(
      class = "co2-inline-help-trigger",
      title = title,
      `aria-label` = title,
      shiny::icon("question-circle")
    ),
    htmltools::tags$div(class = "co2-inline-help-content", ...)
  )
}

# Compact question-mark help placed beside an individual setting label.
.co2_field_help <- function(title = "Help", ...) {
  htmltools::tags$details(
    class = "co2-field-help",
    htmltools::tags$summary(
      class = "co2-field-help-trigger",
      title = title,
      `aria-label` = title,
      shiny::icon("question-circle")
    ),
    htmltools::tags$div(class = "co2-field-help-content", ...)
  )
}

.co2_field_label <- function(label, title = label, ...) {
  htmltools::tags$div(
    class = "co2-field-label",
    htmltools::tags$span(label),
    .co2_field_help(title, ...)
  )
}

.co2_section_heading <- function(label, title = label, ...) {
  htmltools::tags$div(
    class = "co2-section-heading",
    shiny::tags$h4(label),
    .co2_field_help(title, ...)
  )
}
