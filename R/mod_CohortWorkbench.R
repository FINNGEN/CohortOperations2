
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

mod_cohortWorkbench_server <- function(id, r_connectionHandlers, r_workbench,  table_editing=TRUE){
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
      r_workbench$cohortsSummaryDatabases |>
        HadesExtras::rectable_cohortsSummary(
          deleteButtonsShinyId = ns("cohortsWorkbenchDeleteButtons"),
          editButtonsShinyId = ns("cohortsWorkbenchEditButtons"))
    })

    #
    # Ask for confirmation when delete button is clicked
    #
    shiny::observeEvent(input$cohortsWorkbenchDeleteButtons, {

      rowNumber <- input$cohortsWorkbenchDeleteButtons$index
      databaseName <- r_workbench$cohortsSummaryDatabases |> purrr::pluck("databaseName", rowNumber)
      cohortName <- r_workbench$cohortsSummaryDatabases |> purrr::pluck("cohortName", rowNumber)
      shortName <- r_workbench$cohortsSummaryDatabases |> purrr::pluck("shortName", rowNumber)

      shinyWidgets::confirmSweetAlert(
        session = session,
        inputId = ns("confirmSweetAlert_CohortsWorkbenchDeleteButtons"),
        type = "question",
        title = "Delete cohort ?",
        text = htmltools::HTML(paste0(
          "Are you sure you want to delete cohort<br>", shortName,": '", cohortName, "'<br>from database<br>'", databaseName, "' ?"
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
        rowNumber <- input$cohortsWorkbenchDeleteButtons$index
        databaseName <- r_workbench$cohortsSummaryDatabases |> purrr::pluck("databaseName", rowNumber)
        cohortId <- r_workbench$cohortsSummaryDatabases |> purrr::pluck("cohortId", rowNumber)
        databaseId <- fct_getDatabaseIdNamesListFromDatabasesHandlers(r_connectionHandlers$databasesHandlers)[[databaseName]]

        cohortTableHandler <- r_connectionHandlers$databasesHandlers[[databaseId]]$cohortTableHandler
        cohortTableHandler$deleteCohorts(as.integer(cohortId))

        r_workbench$cohortsSummaryDatabases <- fct_getCohortsSummariesFromDatabasesHandlers(r_connectionHandlers$databasesHandlers)

      }
    })


    #
    # Show modal when edit button is clicked
    #
    shiny::observeEvent(input$cohortsWorkbenchEditButtons, {

      rowNumber <- input$cohortsWorkbenchEditButtons$index
      databaseId <- r_workbench$cohortsSummaryDatabases[["databaseId"]][rowNumber]
      cohortName <- r_workbench$cohortsSummaryDatabases[["cohortName"]][rowNumber]
      shortName <- r_workbench$cohortsSummaryDatabases[["shortName"]][rowNumber]

      r_tmp$cohortNames <- r_workbench$cohortsSummaryDatabases |> dplyr::filter({{databaseId}} == databaseId) |> dplyr::pull("cohortName") |> setdiff(cohortName)
      r_tmp$shortNames <- r_workbench$cohortsSummaryDatabases |> dplyr::filter({{databaseId}} == databaseId) |> dplyr::pull("shortName") |> setdiff(shortName)

      shiny::showModal(
        shiny::modalDialog(
          shiny::tags$h4("Edit cohort ", shortName, ":", cohortName, " in database ", databaseId),
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
      rowNumber <- input$cohortsWorkbenchEditButtons$index
      databaseId <- r_workbench$cohortsSummaryDatabases[["databaseId"]][rowNumber]
      cohortId <- r_workbench$cohortsSummaryDatabases[["cohortId"]][rowNumber]

      newShortName <- input$editShortName_textInput
      newCohortName <- input$editCohortName_textInput

      cohortTableHandler <- r_connectionHandlers$databasesHandlers[[databaseId]]$cohortTableHandler
      cohortTableHandler$updateCohortNames(cohortId, newCohortName, newShortName)

      r_workbench$cohortsSummaryDatabases <- fct_getCohortsSummariesFromDatabasesHandlers(r_connectionHandlers$databasesHandlers)

      shiny::removeModal()
    })





  })
}
