#' Cohort Workbench UI Module
#'
#' This module provides the UI for the Cohort Workbench.
#'
#' @param id A unique identifier for the module.
#'
#' @return A UI definition for the Cohort Workbench.
#' 
#' @importFrom shiny NS
#' @importFrom htmltools tagList
#' @importFrom shinyWidgets useSweetAlert
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyFeedback useShinyFeedback
#' @importFrom reactable reactableOutput
#' 
#' @export
mod_cohortWorkbench_ui <- function(id){
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyWidgets::useSweetAlert(),
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    #
    reactable::reactableOutput(ns("cohortsSummaryDatabases_reactable"))
  )
}
#' Cohort Workbench Server Module
#'
#' This module provides the server logic for the Cohort Workbench.
#'
#' @param id A unique identifier for the module.
#' @param r_databaseConnection A reactiveValues object containing the database connection and handlers.
#' @param table_editing A logical value indicating whether table editing is enabled. Default is TRUE.
#'
#' @return A server module for the Cohort Workbench.
#' 
#' @importFrom shiny moduleServer reactiveValues observeEvent req showModal modalDialog textInput actionButton modalButton removeModal
#' @importFrom shinyWidgets confirmSweetAlert
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback
#' @importFrom shinyjs toggleState
#' @importFrom reactable renderReactable
#' @importFrom purrr pluck
#' @importFrom dplyr pull setdiff
#' 
#' @export
mod_cohortWorkbench_server <- function(id, r_databaseConnection, table_editing = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    r_tmp <- shiny::reactiveValues(
      cohortNames = NULL,
      shortNames = NULL
    )

    #
    # Renders cohortsSummaryDatabases_reactable
    #
    output$cohortsSummaryDatabases_reactable <- reactable::renderReactable({
      shiny::req(r_databaseConnection$cohortTableHandler)
      shiny::req(r_databaseConnection$hasChangeCounter)

      r_databaseConnection$cohortTableHandler$getCohortsSummary() |>
        HadesExtras::rectable_cohortsSummary(
          deleteButtonsShinyId = ns("cohortsWorkbenchDeleteButtons"),
          editButtonsShinyId = ns("cohortsWorkbenchEditButtons"))
    })

    #
    # Ask for confirmation when delete button is clicked
    #
    shiny::observeEvent(input$cohortsWorkbenchDeleteButtons, {

      cohortsSummary <- r_databaseConnection$cohortTableHandler$getCohortsSummary()
      rowNumber <- input$cohortsWorkbenchDeleteButtons$index

      cohortName <- cohortsSummary |> purrr::pluck("cohortName", rowNumber)
      shortName <- cohortsSummary |> purrr::pluck("shortName", rowNumber)

      shinyWidgets::confirmSweetAlert(
        session = session,
        inputId = ns("confirmSweetAlert_CohortsWorkbenchDeleteButtons"),
        type = "question",
        title = "Delete cohort ?",
        text = htmltools::HTML(paste0(
          "Are you sure you want to delete cohort<br>", shortName,": '", cohortName, "' ?"
        )),
        btn_labels = c("Cancel", "Delete"),
        html = TRUE
      )

    })

    #
    # If delete confirmation accepted, deletes cohort and updates r_workbench
    #
    shiny::observeEvent(input$confirmSweetAlert_CohortsWorkbenchDeleteButtons, {
      if (input$confirmSweetAlert_CohortsWorkbenchDeleteButtons == TRUE) {

        cohortsSummary <- r_databaseConnection$cohortTableHandler$getCohortsSummary()
        rowNumber <- input$cohortsWorkbenchDeleteButtons$index

        databaseName <- cohortsSummary |> purrr::pluck("databaseName", rowNumber)
        cohortId <- cohortsSummary |> purrr::pluck("cohortId", rowNumber)

        r_databaseConnection$cohortTableHandler$deleteCohorts(as.integer(cohortId))
        r_databaseConnection$hasChangeCounter = r_databaseConnection$hasChangeCounter + 1

      }
    })


    #
    # Show modal when edit button is clicked
    #
    shiny::observeEvent(input$cohortsWorkbenchEditButtons, {

      cohortsSummary <- r_databaseConnection$cohortTableHandler$getCohortsSummary()
      rowNumber <- input$cohortsWorkbenchEditButtons$index

      cohortName <- cohortsSummary |> purrr::pluck("cohortName", rowNumber)
      shortName <- cohortsSummary |> purrr::pluck("shortName", rowNumber)

      r_tmp$cohortNames <- cohortsSummary |> dplyr::pull("cohortName") |> setdiff(cohortName)
      r_tmp$shortNames <- cohortsSummary |> dplyr::pull("shortName") |> setdiff(shortName)

      shiny::showModal(
        shiny::modalDialog(
          shiny::tags$h4("Edit cohort ", shortName, ":", cohortName),
          shiny::textInput(ns("editShortName_textInput"), "Short name", value = shortName),
          shiny::textInput(ns("editCohortName_textInput"), "Cohort name", value = cohortName),
          shiny::actionButton(ns("editCohort_actionButton"), "Save"),
          shiny::modalButton("Cancel"),
          footer = NULL
        )
      )
    })

    #
    # shinyFeedback on the edit cohort modal
    #
    shiny::observeEvent(c(input$editShortName_textInput,input$editCohortName_textInput), {

      activeAcceptButton <- TRUE

      if (input$editShortName_textInput %in% r_tmp$shortNames) {
        shinyFeedback::showFeedbackDanger( inputId = "editShortName_textInput", text = "Short name exists" )
        activeAcceptButton <- FALSE
      }else if (nchar(input$editShortName_textInput) > 10) {
        shinyFeedback::showFeedbackDanger( inputId = "editShortName_textInput", text = "Short name cannot be more than 10 characters" )
        activeAcceptButton <- FALSE
      }else{
        shinyFeedback::hideFeedback(inputId =  "editShortName_textInput")
      }

      if (input$editCohortName_textInput %in% r_tmp$cohortNames) {
        shinyFeedback::showFeedbackDanger( inputId = "editCohortName_textInput", text = "Cohort name exists" )
        activeAcceptButton <- FALSE
      }else if (nchar(input$editShortName_textInput) > 500) {
        shinyFeedback::showFeedbackDanger( inputId = "editCohortName_textInput", text = "Short name cannot be more than 500 characters" )
        activeAcceptButton <- FALSE
      }else{
        shinyFeedback::hideFeedback(inputId =  "editCohortName_textInput")
      }

      shinyjs::toggleState("editCohort_actionButton", condition = activeAcceptButton )
    })

    #
    # Save cohort when editCohort_actionButton is clicked
    #
    shiny::observeEvent(input$editCohort_actionButton, {

      cohortsSummary <- r_databaseConnection$cohortTableHandler$getCohortsSummary()
      rowNumber <- input$cohortsWorkbenchEditButtons$index

      cohortId <- cohortsSummary |> purrr::pluck("cohortId", rowNumber)

      newShortName <- input$editShortName_textInput
      newCohortName <- input$editCohortName_textInput

      r_databaseConnection$cohortTableHandler$updateCohortNames(cohortId, newCohortName, newShortName)
      r_databaseConnection$hasChangeCounter = r_databaseConnection$hasChangeCounter + 1

      shiny::removeModal()
    })





  })
}
