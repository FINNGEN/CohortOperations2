
#' Drag and Drop UI Module
#'
#' This module creates the UI for a drag and drop interface to define cohort operations.
#'
#' @param id A unique identifier for the module.
#' @param testing A boolean indicating whether to include testing elements in the UI.
#'
#' @return A UI definition for the drag and drop module.
#' 
#' @importFrom shiny NS tagList uiOutput textOutput
#' @importFrom shinyjs useShinyjs
#' 
#' @export
mod_dragAndDrop_ui <- function(id, testing = FALSE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::uiOutput(ns("operation_expression")),
    # if testing add this line
    if(testing) shiny::textOutput(ns("test_displayOperationString"))
  )
}

#' Drag and Drop Server Module
#'
#' This module creates the server logic for a drag and drop interface to define cohort operations.
#'
#' @param id A unique identifier for the module.
#' @param r_databaseConnection A reactive database connection object.
#'
#' @return A reactive expression that calculates the cohort operation expression.
#' 
#' @importFrom shiny moduleServer reactive req renderUI renderText
#' @importFrom dplyr select
#' @importFrom htmltools tagList
#' @importFrom shinydashboard box
#' @importFrom shinyjqui orderInput
#' 
#' @export
mod_dragAndDrop_server <- function(id, r_databaseConnection) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    placeholder <-  "Drag and Drop COHORTS and OPERATORS here to create an expression"

    #
    # Update is not working, we build the ui in server
    #
    output$operation_expression <- shiny::renderUI({
      shiny::req(r_databaseConnection$cohortTableHandler)
      shiny::req(r_databaseConnection$hasChangeCounter)

      cohortData <- r_databaseConnection$cohortTableHandler$getCohortsSummary() |>
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
    rf_operationExpression <- shiny::reactive({
      shiny::req(r_databaseConnection$cohortTableHandler)
      shiny::req(r_databaseConnection$hasChangeCounter)
      shiny::req(input$dest_boxes)
      shiny::req(input$dest_boxes != placeholder)

      cohortData <- r_databaseConnection$cohortTableHandler$getCohortsSummary() |>
        dplyr::select(cohortId, cohortName, shortName)

      operation_expression <- input$dest_boxes

      for(i in 1:nrow(cohortData)) {
        operation_expression[which(operation_expression == cohortData$shortName[i])] <- cohortData$cohortId[i]
      }

      paste(operation_expression, collapse = " ")
    })

    #
    # calculates cohort operation expression (just for testing)
    #
    output$test_displayOperationString <- shiny::renderText({
      req(rf_operationExpression())
      rf_operationExpression()
    })

    #
    # returns the operation expresion
    #
    return(rf_operationExpression)
  })
}
