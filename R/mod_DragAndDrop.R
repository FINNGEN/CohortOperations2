

mod_dragAndDrop_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::uiOutput(ns("operation_expression")),
#    shiny::textOutput(ns("dest_boxes")),
  )}


mod_dragAndDrop_server <- function(id, cohort_list) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (!is.null(cohort_list)) {
      cohortData <- cohort_list()
    } else {
      cohortData <- NULL
    }

    output$operation_expression <- shiny::renderUI({
      htmltools::tagList(
        shinydashboard::box(
          title = "Expression defining the cohort",
          width = 12,
          background = "light-blue",
          shinyjqui::orderInput(
            inputId = ns("dest_boxes"),
            width = "100%",
            label = NULL,
            items = NULL, placeholder = "Drag and Drop COHORTS and OPERATORS here to create an expression",
          )
        ),
        shinydashboard::box(
          width = 12,
          shinyjqui::orderInput(
            inputId = ns("source_boxes_cohorts"),
            width = "100%",
            label = "COHORTS",
            items = if(!is.null(cohortData)) cohortData$shortName,
            as_source = TRUE, connect = ns("dest_boxes")
          ),
          shinyjqui::orderInput(
            inputId = ns("source_boxes"),
            width = "100%",
            label = "OPERATORS",
            items = c(`(`="(", `)`=")",
                      ` if they are also in `="&",
                      ` if they are not in `="&!",
                      ` or in ` = "|"),
            as_source = TRUE, connect = ns("dest_boxes")
          )
        )

      )
    })

    #
    # calculates cohort operation expression (just for testing)
    #
    output$dest_boxes <- shiny::renderText({
      req(!is.null(input$dest_boxes))
      req(!is.null(cohortData))

      operation_expression <- input$dest_boxes
      for(i in 1:nrow(cohortData)) {
        operation_expression[which(operation_expression == cohortData$shortName[i])] <- i
      }

      paste(operation_expression, collapse = "")
    })
  })


  #
  # calculates cohort operation expression
  #
  shiny::reactive({
    req(!is.null(input$dest_boxes))
    req(!is.null(cohortData))

    operation_expression <- input$dest_boxes
    for(i in 1:nrow(cohortData)) {
      operation_expression[which(operation_expression == cohortData$shortName[i])] <- i
    }

    paste(operation_expression, collapse = "")
  })


}
