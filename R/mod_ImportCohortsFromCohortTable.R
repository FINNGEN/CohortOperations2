#' Import Cohorts from Cohort Table UI
#'
#' @param id Module ID
#'
#' @return A UI definition for the Import Cohorts from Cohort Table module
#'
#' @importFrom shiny NS actionButton
#' @importFrom shinyjs useShinyjs
#' @importFrom htmltools tagList hr
#' @importFrom reactable reactableOutput
#'
#' @export
mod_importCohortsFromCohortsTable_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    mod_fct_appendCohort_ui(),
    shinyjs::useShinyjs(),
    htmltools::hr(),
    reactable::reactableOutput(ns("cohorts_reactable")) |> ui_load_spinner(),
    htmltools::hr(),
    shiny::actionButton(ns("import_actionButton"), "Import Selected")
    # toggle import_actionButton
  )
}

#' Import Cohorts from Cohort Table Server
#'
#' @param id Module ID
#' @param r_databaseConnection Reactive database connection object
#' @param filterCohortsRegex Regular expression to filter cohorts
#' @param filterCohortsRegexRemove Regular expression to remove cohorts from filter
#' @param filterCohortsName Name filter for cohorts
#'
#' @return Server logic for the Import Cohorts from Cohort Table module
#'
#' @importFrom shiny moduleServer reactiveValues observe observeEvent req validate need
#' @importFrom shinyjs toggleState
#' @importFrom reactable renderReactable getReactableState updateReactable
#' @importFrom dplyr arrange slice pull
#' @importFrom ParallelLogger logInfo logWarn
#' @importFrom HadesExtras checkCohortDefinitionTables getCohortNamesFromCohortDefinitionTable cohortTableToCohortDefinitionSettings
#'
#' @export
mod_importCohortsFromCohortsTable_server <- function(
    id,
    r_databaseConnection,
    filterCohortsRegex = ".*",
    filterCohortsRegexRemove = "()", # default to remove nothing
    filterCohortsName = "Endpoint") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      selectedIndex = NULL,
      cohortDefinitionTable = NULL
    )

    r_cohortDefinitionSetToAdd <- shiny::reactiveValues(
      cohortDefinitionSet = NULL
    )

    #
    # get cohorts table cohortDefinitionTable
    #
    shiny::observe({
      shiny::req(r_databaseConnection$cohortTableHandler)

      ParallelLogger::logInfo("[Import from Cohort Table-", filterCohortsRegex, "] : Getting cohort names from cohort table")

      # get connection
      connection <- r_databaseConnection$cohortTableHandler$connectionHandler$getConnection()
      cohortDatabaseSchema <- r_databaseConnection$cohortTableHandler$cdmDatabaseSchema

      ParallelLogger::logInfo("[Import from Cohort Table-", filterCohortsRegex, "] : Getting cohort names from cohort table from:", cohortDatabaseSchema)

      logTibble <- HadesExtras::checkCohortDefinitionTables(
        connection = connection,
        cohortDatabaseSchema = cohortDatabaseSchema
      )

      thereIsCohortTables <- (logTibble$logTibble$type[1] == "ERROR" | logTibble$logTibble$type[2] == "ERROR")

      if (thereIsCohortTables) {
        ParallelLogger::logWarn("[Import from Cohort Table-", filterCohortsRegex, "] : Error connecting to Endpoint table.")
        r$cohortDefinitionTable <- "Error connecting to Endpoint table."
        return()
      }

      cohortDefinitionTable <- HadesExtras::getCohortNamesFromCohortDefinitionTable(
        connection = connection,
        cohortDatabaseSchema = cohortDatabaseSchema
      ) |> dplyr::arrange(cohort_definition_name)

      r$cohortDefinitionTable <- cohortDefinitionTable
    })

    #
    # If cohortDefinitionTable is available, render the reactable
    #
    output$cohorts_reactable <- reactable::renderReactable({
      cohortDefinitionTable <- r$cohortDefinitionTable

      if (is.character(cohortDefinitionTable)) {
        ParallelLogger::logWarn("[Import from cohort table-", filterCohortsRegex, "] : ", cohortDefinitionTable)
      }
      shiny::validate(shiny::need(!is.character(cohortDefinitionTable), cohortDefinitionTable))

      columns <- list(
        cohort_definition_id = reactable::colDef(show = FALSE),
        cohort_definition_name = reactable::colDef(name = paste0(filterCohortsName, " Name")),
        cohort_definition_description = reactable::colDef(name = paste0(filterCohortsName, " Description"))
      )
      
  
      cohortDefinitionTable |>
        dplyr::filter(grepl(filterCohortsRegex, cohort_definition_name, perl = TRUE)) |>
        dplyr::mutate(cohort_definition_name = stringr::str_remove(cohort_definition_name, filterCohortsRegexRemove)) |>
        reactable::reactable(
          columns = columns,
          selection = "multiple",
          onClick = "select",
          searchable = TRUE
        )
    })
    # Copy to reactive variable, (better than reactive value for testing)
    shiny::observe({
      selectedIndex <- reactable::getReactableState("cohorts_reactable", "selected", session)
      r$selectedIndex <- selectedIndex
    })

    #
    # button import selected: checks selected cohorts
    #
    observe({
      shinyjs::toggleState("import_actionButton", condition = !is.null(r$selectedIndex))
    })

    shiny::observeEvent(input$import_actionButton, {
      shiny::req(r$selectedIndex)

      fct_sweetAlertSpinner("Processing cohorts")

      # get connection
      connection <- r_databaseConnection$cohortTableHandler$connectionHandler$getConnection()
      cohortDatabaseSchema <- r_databaseConnection$cohortTableHandler$cdmDatabaseSchema

      cohortDefinitionTable <- r$cohortDefinitionTable
      selectedCohortIds <- cohortDefinitionTable |>
        dplyr::slice(r$selectedIndex) |>
        dplyr::pull(cohort_definition_id)

      # calculate new cohorIds
      numberNewCohorts <- length(selectedCohortIds)
      unusedCohortIds <- setdiff(1:1000, r_databaseConnection$cohortTableHandler$cohortDefinitionSet$cohortId) |> head(numberNewCohorts)

      cohortDefinitionSet <- HadesExtras::cohortTableToCohortDefinitionSettings(
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortDefinitionTable = cohortDefinitionTable,
        cohortDefinitionIds = selectedCohortIds,
        newCohortDefinitionIds = unusedCohortIds
      )

      r_cohortDefinitionSetToAdd$cohortDefinitionSet <- cohortDefinitionSet

      ParallelLogger::logInfo(
        "[Import from Cohort Table-", filterCohortsRegex, "] Importing cohorts: ", r_cohortDefinitionSetToAdd$cohortDefinitionSet$cohortName,
        " with ids: ", r_cohortDefinitionSetToAdd$cohortDefinitionSet$cohortId
      )

      fct_removeSweetAlertSpinner()
    })

    #
    # evaluate the cohorts to append; if accepted increase output to trigger closing actions
    #
    rf_append_accepted_counter <- mod_fct_appendCohort_server("import_atlas", r_databaseConnection, r_cohortDefinitionSetToAdd)

    # close and reset
    shiny::observeEvent(rf_append_accepted_counter(), {
      r_cohortDefinitionSetToAdd$cohortDefinitionSet <- NULL
      reactable::updateReactable("cohorts_reactable", selected = NA, session = session)
    })
  })
}
