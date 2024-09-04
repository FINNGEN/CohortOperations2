
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

mod_cohortWorkbench_server <- function(id, r_connectionHandler,  table_editing=TRUE){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns


    r_tmp <- shiny::reactiveValues(
      cohortNames = NULL,
      shortNames = NULL
    )

    #
    # Renders cohortsSummaryDatabases_reactable
    #
    output$cohortsSummaryDatabases_reactable <- reactable::renderReactable({
      shiny::req(r_connectionHandler$cohortTableHandler)
      shiny::req(r_connectionHandler$hasChangeCounter)

      r_connectionHandler$cohortTableHandler$getCohortsSummary() |>
        HadesExtras::rectable_cohortsSummary(
          deleteButtonsShinyId = ns("cohortsWorkbenchDeleteButtons"),
          editButtonsShinyId = ns("cohortsWorkbenchEditButtons"))
    })

    #
    # Ask for confirmation when delete button is clicked
    #
    shiny::observeEvent(input$cohortsWorkbenchDeleteButtons, {

      cohortsSummary <- r_connectionHandler$cohortTableHandler$getCohortsSummary()
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

        cohortsSummary <- r_connectionHandler$cohortTableHandler$getCohortsSummary()
        rowNumber <- input$cohortsWorkbenchDeleteButtons$index

        databaseName <- cohortsSummary |> purrr::pluck("databaseName", rowNumber)
        cohortId <- cohortsSummary |> purrr::pluck("cohortId", rowNumber)

        r_connectionHandler$cohortTableHandler$deleteCohorts(as.integer(cohortId))
        r_connectionHandler$hasChangeCounter = r_connectionHandler$hasChangeCounter + 1

      }
    })


    #
    # Show modal when edit button is clicked
    #
    shiny::observeEvent(input$cohortsWorkbenchEditButtons, {

      cohortsSummary <- r_connectionHandler$cohortTableHandler$getCohortsSummary()
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

      cohortsSummary <- r_connectionHandler$cohortTableHandler$getCohortsSummary()
      rowNumber <- input$cohortsWorkbenchEditButtons$index

      cohortId <- cohortsSummary |> purrr::pluck("cohortId", rowNumber)

      newShortName <- input$editShortName_textInput
      newCohortName <- input$editCohortName_textInput

      r_connectionHandler$cohortTableHandler$updateCohortNames(cohortId, newCohortName, newShortName)
      r_connectionHandler$hasChangeCounter = r_connectionHandler$hasChangeCounter + 1

      shiny::removeModal()
    })





  })
}
