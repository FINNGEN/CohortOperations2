

mod_formTimeWindows_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h4("Time windows"),
    shiny::tags$div(style = "margin-left: 50px; margin-right: 100px; min-width: 600px;",
                    # shiny::tags$div(id=ns('inputList')),
                    # shiny::actionButton(ns('addBtn'), 'Add Window'),
                    shiny::br(),
                    shiny::fixedRow(
                      style = "margin-bottom: 130px;",
                      shiny::column(
                        2, shiny::absolutePanel(
                          width = "300px",
                          shiny::numericInput(
                            ns("years_before"),
                            "Years before", min = -100, max = -1, value = -5, width = "33%"))
                      ),
                      shiny::column(
                        2,
                        shiny::absolutePanel(
                          width = "300px",
                          shiny::numericInput(
                            ns("years_after"),
                            "Years after", min = 1, max = 100, value = 5, width = "33%"))
                      ),
                      shiny::column(
                        2,
                        shiny::absolutePanel(
                          width = "300px",
                          shiny::numericInput(
                            ns("windows"), "Windows", min = 1, max = 20, value = 4, width = "33%"))
                      ),
                      shiny::column(
                        3, offset = 0,  style="margin-top:25px; margin-bottom:-20px;",
                        shinyWidgets::awesomeCheckbox(
                          inputId = ns("zero_window"),
                          label = "Add day 0-0 window",
                          status = "primary",
                          value = TRUE
                        )
                      )
                    ),
                    shiny::uiOutput(ns("slider.ui")),
                    shiny::br(),
                    shiny::fixedRow(
                      shiny::column(
                        3, offset = 0,
                        shiny::tags$h5("Breakpoints"),
                        shiny::tableOutput(ns("slider_value"))
                      ),
                      shiny::column(
                        3, offset = 3,
                        shiny::tags$h5("Window sizes"),
                        shiny::tableOutput(ns("slider_distance"))
                      )
                    ),
                    shiny::br(),
                    shiny::actionButton(ns("slider_help_button"), "Help"),
                    shinyjs::hidden(
                      shiny::verbatimTextOutput(ns("slider_help"))
                    )
    )
  )
}


mod_formTimeWindows_server <- function(id, session) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    days_before <- shiny::reactive({ floor(input$years_before * 365.25) })
    days_after <- shiny::reactive({ ceiling(input$years_after * 365.25) })

    windows <- function(n){
      shiny::req(days_before(), days_after())

      c( round( seq(days_before(), days_after(), (days_after() - days_before()) / n) ) )
    }

    distance_table <- function(x, col_names){
      dt <- tibble::tibble(x,
                           round(lubridate::days(x)/lubridate::weeks(1), 1),
                           lubridate::days(x)/months(1),
                           lubridate::days(x)/lubridate::years(1)) |>
        dplyr::mutate(row = dplyr::row_number(), .before = starts_with("x"))
      names(dt) <- col_names
      dt
    }

    shiny::observeEvent(c(days_before, days_after), {
      shinyWidgets::updateNoUiSliderInput(
        inputId = ns("the_slider"),
        range = c(days_before(), days_after())
      )
    })

    output$slider_value <- shiny::renderTable({
      shiny::req(input$the_slider)
      distance_table(input$the_slider, c("Break", "Days", "Weeks", "Months", "Years"))
    }, digits = 1)

    output$slider_distance <- shiny::renderTable({
      shiny::req(input$the_slider)
      distance_table(diff(input$the_slider), c("Window", "Days", "Weeks", "Months", "Years"))
    }, digits = 1)

    #
    # slider.ui
    #
    output$slider.ui <- shiny::renderUI({
      shiny::req(input$windows, days_before(), days_after())

      shiny::validate(
        shiny::need(days_before() < 0, "'Years before' needs to be negative"),
        shiny::need(days_after() > 0, "'Years after' needs to be positive"),
        shiny::need(input$windows > 0, "'Windows' needs to be positive")
      )

      shinyWidgets::noUiSliderInput(
        inputId = ns("the_slider"),
        label = NULL,
        min = days_before(),
        max = days_after(),
        step = 1,
        value = windows(input$windows),
        format = shinyWidgets::wNumbFormat(decimals = 0, thousand = "", prefix = ""),
        pips = list(
          mode = 'positions',
          values = list(0, (100 * abs(days_before()) / (days_after() - (days_before()))), 100),
          density = 10
        ),
        behaviour =  c("none"),
        color = "#a2dbff",
        update_on = "change", # end, change
        width = "100%"
      )
    })

    output$slider_help <- shiny::renderText({
      paste(
        "You can move the slider thumbs by dragging or by using the arrow keys.",
        "The 'Day 0-0 window' is a special case, and is included by default.",
        sep = "\n"
      )
    })

    shiny::observeEvent(input$slider_help_button, {
      shinyjs::toggle(id = "slider_help")
    })

    #
    # reactive for the time windows
    #
    shiny::reactive({
      shiny::req(input$the_slider)
      breaks <- as.vector(input$the_slider)

      if(input$zero_window){
        list(
          temporalStartDays = c(0, breaks[1:(length(breaks) - 1)]),
          temporalEndDays = c(0, breaks[2:length(breaks)])
        )
      } else {
        list(
          temporalStartDays = breaks[1:(length(breaks) - 1)],
          temporalEndDays = breaks[2:length(breaks)]
        )
      }
    })


  })
}
