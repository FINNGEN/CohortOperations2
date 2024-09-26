#' @title mod_selectDatabases_ui
#'
#' @description UI module for selecting and connecting to databases.
#'
#' @param id Shiny module ID.
#'
#' @importFrom shiny NS tagList h2 br p checkboxInput h4
#' @importFrom shinyWidgets useSweetAlert pickerInput
#' @importFrom reactable reactableOutput
#'
#' @return Shiny UI elements for the database selection module.
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

#' @title mod_selectDatabases_server
#'
#' @description Server module for handling database selection and connection.
#'
#' @param id Shiny module ID.
#' @param databasesConfig Configuration list for databases.
#' @param r_databaseConnection Reactive values for database connection.
#'
#' @importFrom shiny moduleServer reactiveValues observe observeEvent req
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom ParallelLogger logInfo
#' @importFrom HadesExtras LogTibble createCohortTableHandlerFromList reactable_connectionStatus
#' @importFrom dplyr mutate relocate
#' @importFrom reactable renderReactable
#'
#' @return Shiny server logic for the database selection module.
mod_selectDatabases_server <- function(id, databasesConfig, r_databaseConnection) {
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

      fct_removeSweetAlertSpinner()
      fct_sweetAlertSpinner("Connecting to databases")

      cohortTableHandlerConfig <- databasesConfig[[input$selectDatabases_pickerInput]]$cohortTableHandler
      atlasConfig <- databasesConfig[[input$selectDatabases_pickerInput]]$atlasConfig
      ParallelLogger::logInfo("[Databases Connection] Connecting to: ", input$selectDatabases_pickerInput)

      loadConnectionChecksLevel <- "allChecks"
      if (!input$allChecks_checkbox) {
        loadConnectionChecksLevel <- "basicChecks"
      }

      cohortTableHandler <- HadesExtras::createCohortTableHandlerFromList(cohortTableHandlerConfig, loadConnectionChecksLevel)
      r_databaseConnection$cohortTableHandler <- cohortTableHandler
      r_databaseConnection$atlasConfig <- atlasConfig
      r_databaseConnection$hasChangeCounter <- r_databaseConnection$hasChangeCounter + 1
      # TEMP, trigger garbage collector to delete the old handlers
      gc()

      connectionStatusLogs <- r_databaseConnection$cohortTableHandler$connectionStatusLog
      ParallelLogger::logInfo("[Databases Connection] Connected to: ", connectionStatusLogs)

      # check Atlas config
      webapiurl <- databasesConfig[[input$selectDatabases_pickerInput]]$atlasConfig$webapiurl
      error <- NULL
      tryCatch({
        dummy  <- ROhdsiWebApi::getCdmSources(webapiurl)
      }, error = function(e) {
        error <<- e$message
      })

      atlasStatusLogs <- tibble::tibble(
        databaseId = cohortTableHandlerConfig$database$databaseId,
        databaseName = cohortTableHandlerConfig$database$databaseName,
        type = "SUCCESS",
        step = "Check Atlas connection",
        message = "Connected"
      )
      if(!is.null(error)){
        atlasStatusLogs <- atlasStatusLogs |>
          dplyr::mutate(
            type = "ERROR",
            message = error
          )
      }
      connectionStatusLogs <- connectionStatusLogs |>
        dplyr::bind_rows(atlasStatusLogs)

      r$connectionStatusLogs <- connectionStatusLogs

    })

    output$connectionStatusLogs_reactable <- reactable::renderReactable({
      fct_removeSweetAlertSpinner()

      r$connectionStatusLogs |>
        HadesExtras::reactable_connectionStatus()

    })


  })
}
