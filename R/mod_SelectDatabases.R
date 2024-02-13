mod_selectDatabases_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # imports
    shinyWidgets::useSweetAlert(),
    #
    shiny::h2("Databases connection"),
    shiny::br(),
    shiny::p("CohortOperations can connect to one or more databases. By default, it will connect only to the top database."),
    shiny::br(),
    shinyWidgets::pickerInput(
      inputId = ns("selectDatabases_pickerInput"),
      label = "Connect to databases:",
      choices = NULL,
      multiple = TRUE),
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


mod_selectDatabases_server <- function(id, configurationList, r_connectionHandlers) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    r <- shiny::reactiveValues(
      connectionStatusLogs = HadesExtras::LogTibble$new()$logTibble |>
        dplyr::mutate(databaseName = "") |>
        dplyr::relocate(databaseName, .before = 1)
    )


    shiny::observe({
      configurationListChecks <- fct_checkConfigurationList(configurationList)

      if (isFALSE(configurationListChecks)) {
        shinyWidgets::sweetAlert(
          session = session,
          title = "Error reading the settings file.",
          text = configurationListChecks,
          type = "error"
        )
      }else{
        databaseIdNamesList <- fct_getDatabaseIdNamesListFromConfigurationList(configurationList)
        r$selectDatabases_pickerInput <- databaseIdNamesList[1]

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "selectDatabases_pickerInput",
          choices = databaseIdNamesList,
          selected = databaseIdNamesList[1]
        )
      }
    })




    shiny::observeEvent(c(input$selectDatabases_pickerInput, input$allChecks_checkbox), {

      sweetAlert_spinner("Connecting to databases")

      selectedConfigurationList <- configurationList[input$selectDatabases_pickerInput]

      loadConnectionChecksLevel <- "allChecks"
      if (!input$allChecks_checkbox) {
        loadConnectionChecksLevel <- "basicChecks"
      }

      databasesHandlers <- fct_configurationListToDatabasesHandlers(selectedConfigurationList, loadConnectionChecksLevel)
      r_connectionHandlers$databasesHandlers <- databasesHandlers

      remove_sweetAlert_spinner()

    })

    shiny::observeEvent(r_connectionHandlers$databasesHandlers, {

      connectionStatusLogs <- HadesExtras::LogTibble$new()$logTibble |>
        dplyr::mutate(databaseName = "") |>
        dplyr::relocate(databaseName, .before = 1)

      for(databaseId in names(r_connectionHandlers$databasesHandlers)){
        connectionStatusLogs <- dplyr::bind_rows(
          connectionStatusLogs,
          r_connectionHandlers$databasesHandlers[[databaseId]]$cohortTableHandler$connectionStatusLog
        )
      }

      r$connectionStatusLogs <- connectionStatusLogs

    })

    output$connectionStatusLogs_reactable <- reactable::renderReactable({

      r$connectionStatusLogs |>
        HadesExtras::reactable_connectionStatus()
    })


  })
}


