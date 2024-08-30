
mod_importCohortsFromCohortsTable_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    mod_fct_appendCohort_ui(),
    shinyjs::useShinyjs(),
    #
    shinyWidgets::pickerInput(
      inputId = ns("selectDatabases_pickerInput"),
      label = "Load patients into database:",
      choices = NULL,
      selected = NULL,
      multiple = FALSE),
    #
    htmltools::hr(),
    reactable::reactableOutput(ns("cohorts_reactable")) |>  ui_load_spinner(),
    htmltools::hr(),
    shiny::actionButton(ns("import_actionButton"), "Import Selected")
    # toggle import_actionButton

  )
}

mod_importCohortsFromCohortsTable_server <- function(id, r_connectionHandlers, r_workbench) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns


    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      cohortDefinitionTable = NULL
    )

    r_cohortDefinitionSetToAdd <- shiny::reactiveValues(
      databaseName = NULL,
      cohortDefinitionSet = NULL
    )

    #
    # render selectDatabases_pickerInput
    #
    shiny::observe({
      shiny::req(r_connectionHandlers$databasesHandlers)

      databaseIdNamesList <- fct_getDatabaseIdNamesListFromDatabasesHandlers(r_connectionHandlers$databasesHandlers)

      shinyWidgets::updatePickerInput(
        inputId = "selectDatabases_pickerInput",
        choices = databaseIdNamesList,
        selected = databaseIdNamesList[1])
    })



    #
    # render cohorts_reactable
    #
    output$cohorts_reactable <- reactable::renderReactable({
      shiny::req(r_connectionHandlers$databasesHandlers)
      shiny::req(input$selectDatabases_pickerInput)

      ParallelLogger::logInfo("[Import from Cohort Table] : Getting cohort names from cohort table")

      # get connection
      connection  <- r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandler$connectionHandler$getConnection()
      cohortDatabaseSchema  <-  r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandler$cdmDatabaseSchema

      ParallelLogger::logInfo("[Import from Cohort Table] : Getting cohort names from cohort table from:", cohortDatabaseSchema)

      logTibble <- HadesExtras::checkCohortDefinitionTables(
        connection = connection,
        cohortDatabaseSchema = cohortDatabaseSchema
      )

      thereIsCohortTables <- (logTibble$logTibble$type[1] == "ERROR" | logTibble$logTibble$type[2] == "ERROR")

      if (thereIsCohortTables) {
        ParallelLogger::logWarn("[Import from Cohort Table] : Error connecting to Endpoint table.")
      }
      shiny::validate(shiny::need(!thereIsCohortTables, "Error connecting to Endpoint table."))

      cohortDefinitionTable <- HadesExtras::getCohortNamesFromCohortDefinitionTable(
        connection = connection,
        cohortDatabaseSchema = cohortDatabaseSchema
      ) |> dplyr::arrange(cohort_definition_name)

      r$cohortDefinitionTable <- cohortDefinitionTable

      columns <- list(
        cohort_definition_id = reactable::colDef( show = FALSE),
        cohort_definition_name = reactable::colDef(name = "Endpoint Name"),
        cohort_definition_description = reactable::colDef(name = "Endpoint Description")
      )

      cohortDefinitionTable |>
        reactable::reactable(
          columns = columns,
          selection = "multiple",
          onClick = "select",
          searchable = TRUE
        )

    })
    # reactive function to get selected values
    r_selectedIndex <- reactive(reactable::getReactableState("cohorts_reactable", "selected", session))

    #
    # button import selected: checks selected cohorts
    #
    observe({
      shinyjs::toggleState("import_actionButton", condition = !is.null(r_selectedIndex()) )
    })

    shiny::observeEvent(input$import_actionButton, {
      shiny::req(r_selectedIndex())

      sweetAlert_spinner("Processing cohorts")

      # get connection
      cohortTableHandler <- r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandler
      connection  <- cohortTableHandler$connectionHandler$getConnection()
      cohortDatabaseSchema  <-  cohortTableHandler$cdmDatabaseSchema

      cohortDefinitionTable <- r$cohortDefinitionTable
      selectedCohortIds <- cohortDefinitionTable |>
        dplyr::slice(r_selectedIndex()) |>
        dplyr::pull(cohort_definition_id)

      # calculate new cohorIds
      numberNewCohorts <- length(selectedCohortIds)
      unusedCohortIds <- setdiff(1:1000, cohortTableHandler$cohortDefinitionSet$cohortId) |> head(numberNewCohorts)

      cohortDefinitionSet  <- HadesExtras::cohortTableToCohortDefinitionSettings(
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortDefinitionTable = cohortDefinitionTable,
        cohortDefinitionIds = selectedCohortIds,
        newCohortDefinitionIds = unusedCohortIds
      )

      r_cohortDefinitionSetToAdd$databaseName <- input$selectDatabases_pickerInput
      r_cohortDefinitionSetToAdd$cohortDefinitionSet <- cohortDefinitionSet

      ParallelLogger::logInfo("[Import from Cohort Table] Importing cohorts: ", r_cohortDefinitionSetToAdd$cohortDefinitionSet$cohortName,
                              " with ids: ", r_cohortDefinitionSetToAdd$cohortDefinitionSet$cohortId,
                              " to database", input$selectDatabases_pickerInput)

      remove_sweetAlert_spinner()
    })

    #
    # evaluate the cohorts to append; if accepted increase output to trigger closing actions
    #
    rf_append_accepted_counter <- mod_fct_appendCohort_server("import_atlas", r_connectionHandlers, r_workbench, r_cohortDefinitionSetToAdd )

    # close and reset
    shiny::observeEvent(rf_append_accepted_counter(), {
      r_cohortDefinitionSetToAdd$cohortDefinitionSet <- NULL
      reactable::updateReactable("cohorts_reactable", selected = NA, session = session )
    })

  })


}
















