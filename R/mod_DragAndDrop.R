

mod_dragAndDrop_ui <- function(id, testing = FALSE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::uiOutput(ns("operation_expression")),
    #if testing add this line
    if(testing) shiny::textOutput(ns("test_displayOperationString"))
  )}


mod_dragAndDrop_server <- function(id, r_workbench) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    placeholder <-  "Drag and Drop COHORTS and OPERATORS here to create an expression"

    #
    # Update is not working, we build the ui in server
    #
    output$operation_expression <- shiny::renderUI({
      shiny::req(r_workbench$cohortsSummaryDatabases)

      cohortData <- r_workbench$cohortsSummaryDatabases |>
        dplyr::select(cohortId, cohortName, shortName)

      htmltools::tagList(
        shinydashboard::box(
          title = "Expression defining the cohort",
          width = 12,
          background = "light-blue",
          shinyjqui::orderInput(
            inputId = ns("dest_boxes"),
            width = "100%",
            label = NULL,
            items = NULL,
            placeholder = placeholder
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
                      ` if they are also in `="Ip",
                      ` if they are not in `="Mp",
                      ` or in ` = "Upd"),
            as_source = TRUE, connect = ns("dest_boxes")
          )
        )

      )
    })

    #
    # calculates cohort operation expression
    #
    rf_operationExpresion <- shiny::reactive({
      shiny::req(r_workbench$cohortsSummaryDatabases)
      shiny::req(input$dest_boxes)
      shiny::req(input$dest_boxes != placeholder)

      cohortData <- r_workbench$cohortsSummaryDatabases |>
        dplyr::select(cohortId, cohortName, shortName)

      operation_expression <- input$dest_boxes
      for(i in 1:nrow(cohortData)) {
        operation_expression[which(operation_expression == cohortData$shortName[i])] <- cohortData$cohortId[i]
      }

      paste(operation_expression, collapse = "")
    })

    #
    # calculates cohort operation expression (just for testing)
    #
    output$test_displayOperationString <- shiny::renderText({
      req(rf_operationExpresion())
      rf_operationExpresion()
    })

    #
    # returns the operation expresion
    #
    return(rf_operationExpresion)
  })
}
