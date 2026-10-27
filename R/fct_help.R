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
