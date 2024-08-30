mod_selectDatabases_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # imports
    shinyWidgets::useSweetAlert(),
    #
    shiny::h2("Databases connection"),
    shiny::br(),
    shiny::p("CohortOperations can connect to one database (at the moment). By default, it will connect only to the top database."),
    shiny::br(),
    shinyWidgets::pickerInput(
      inputId = ns("selectDatabases_pickerInput"),
      label = "Connect to database:",
      choices = NULL,
      multiple = FALSE),
    shiny::checkboxInput(
      inputId = ns("allChecks_checkbox"),
      label = "Perform all the checks on the databases (Slow, only for debugging)",
      value = FALSE),
    shiny::br(),
    shiny::h4("Connection status"),
    shiny::p("This table shows the connected databases."),
    reactable::reactableOutput(ns("connectionStatusLogs_reactable"))
  )
}


mod_selectDatabases_server <- function(id, databasesConfig, r_connectionHandler) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    r <- shiny::reactiveValues(
      connectionStatusLogs = HadesExtras::LogTibble$new()$logTibble |>
        dplyr::mutate(databaseName = "") |>
        dplyr::relocate(databaseName, .before = 1)
    )

    #
    # update selectDatabases_pickerInput with database names
    #
    shiny::observe({
      databaseIdNamesList <- list()
      for(databaseId in names(databasesConfig)){
        databaseIdNamesList[[databasesConfig[[databaseId]]$cohortTableHandler$database$databaseName]] <- databaseId
      }

      r$selectDatabases_pickerInput <- databaseIdNamesList[1]

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "selectDatabases_pickerInput",
        choices = databaseIdNamesList,
        selected = databaseIdNamesList[1]
      )
    })

    shiny::observeEvent(c(input$selectDatabases_pickerInput,input$allChecks_checkbox), {
      shiny::req(input$selectDatabases_pickerInput)

      remove_sweetAlert_spinner()
      sweetAlert_spinner("Connecting to databases")

      cohortTableHandlerConfig <- databasesConfig[[input$selectDatabases_pickerInput]]$cohortTableHandler
      ParallelLogger::logInfo("[Databases Connection] Connecting to: ", input$selectDatabases_pickerInput)

      loadConnectionChecksLevel <- "allChecks"
      if (!input$allChecks_checkbox) {
        loadConnectionChecksLevel <- "basicChecks"
      }

      cohortTableHandler <- HadesExtras::createCohortTableHandlerFromList(cohortTableHandlerConfig, loadConnectionChecksLevel)
      r_connectionHandler$cohortTableHandler <- cohortTableHandler
      r_connectionHandler$hasChangeCounter <- r_connectionHandler$hasChangeCounter + 1
      # TEMP, trigger garbage collector to delete the old handlers
      gc()

      remove_sweetAlert_spinner()

    })

    shiny::observeEvent(r_connectionHandler$cohortTableHandler, {
      shiny::req(input$selectDatabases_pickerInput)

      connectionStatusLogs <- r_connectionHandler$cohortTableHandler$connectionStatusLog

      ParallelLogger::logInfo("[Databases Connection] Connected to: ", connectionStatusLogs)
      r$connectionStatusLogs <- connectionStatusLogs

    })

    output$connectionStatusLogs_reactable <- reactable::renderReactable({

      r$connectionStatusLogs |>
        HadesExtras::reactable_connectionStatus()
    })


  })
}


