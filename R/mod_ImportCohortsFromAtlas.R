#' Import Cohorts from Atlas UI Module
#'
#' This module provides the UI for importing cohorts from Atlas.
#'
#' @param id A unique identifier for the module.
#'
#' @return A UI definition for the module.
#' 
#' @importFrom shiny NS actionButton moduleServer reactiveValues 
#' @importFrom shinyjs useShinyjs 
#' @importFrom htmltools tagList hr
#' @importFrom reactable reactableOutput 
#' 
#' @export
mod_importCohortsFromAtlas_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    mod_fct_appendCohort_ui(),
    shinyjs::useShinyjs(),
    htmltools::hr(),
    shiny::actionButton(ns("refreshDatabases_actionButton"), "Refresh Cohort List"),
    reactable::reactableOutput(ns("cohorts_reactable")) |>  ui_load_spinner(),
    htmltools::hr(),
    shiny::actionButton(ns("import_actionButton"), "Import Selected")
    # toggle import_actionButton
  )
}

#' Import Cohorts from Atlas Server Module
#'
#' This module provides the server logic for importing cohorts from Atlas.
#'
#' @param id A unique identifier for the module.
#' @param r_databaseConnection A reactive database connection object.
#' @param filterCohortsRegex A regex pattern to filter cohorts by name.
#'
#' @return None
#' 
#' @importFrom shiny moduleServer reactiveValues observe observeEvent req validate need
#' @importFrom shinyjs toggleState
#' @importFrom reactable renderReactable getReactableState updateReactable
#' @importFrom dplyr filter arrange desc select slice pull
#' @importFrom ParallelLogger logInfo logWarn
#' @importFrom ROhdsiWebApi getCohortDefinitionsMetaData exportCohortDefinitionSet
#' 
#' @export
mod_importCohortsFromAtlas_server <- function(id, r_databaseConnection, filterCohortsRegex='*') {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      selectedIndex = NULL,
      atlasCohortsTable = NULL
    )

    r_cohortDefinitionSetToAdd <- shiny::reactiveValues(
      cohortDefinitionSet = NULL
    )

    #
    # render cohorts_reactable
    #
    shiny::observe({
      shiny::req(r_databaseConnection$cohortTableHandler)
      input$refreshDatabases_actionButton

      webApiUrl <- r_databaseConnection$atlasConfig$webapiurl

      r$atlasCohortsTable  <- NULL

      ParallelLogger::logInfo("[Import from Atlas-", filterCohortsRegex,"] Load from url: ", webApiUrl)

      atlasCohortsTable <- NULL
      tryCatch({
        atlasCohortsTable <- ROhdsiWebApi::getCohortDefinitionsMetaData(webApiUrl) |>
        dplyr::filter(grepl(filterCohortsRegex, name)) |>
        dplyr::arrange(dplyr::desc(id)) |>
        dplyr::select(id, name, description)
      }, error = function(e) {
        atlasCohortsTable <<- paste("Error connecting to Atlas. Check that Atlas is working.")
      })

      r$atlasCohortsTable <- atlasCohortsTable
    })


    output$cohorts_reactable <- reactable::renderReactable({
      atlasCohortsTable <- r$atlasCohortsTable

      if (is.character(atlasCohortsTable)) {
        ParallelLogger::logWarn("[Import from Atlas-", filterCohortsRegex,"] : ", atlasCohortsTable)
      }
      shiny::validate(shiny::need(!is.character(atlasCohortsTable), atlasCohortsTable))

      colums <- list(
        id = reactable::colDef(name = "Cohort ID", show = (filterCohortsRegex == '*') ),
        name = reactable::colDef(name = "Cohort Name"),
        description = reactable::colDef(name = "Description")

      )

      atlasCohortsTable |>
        reactable::reactable(
          columns = colums,
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
      shinyjs::toggleState("import_actionButton", condition = !is.null(r$selectedIndex) )
    })

    shiny::observeEvent(input$import_actionButton, {
      shiny::req(r$atlasCohortsTable)
      shiny::req(r$selectedIndex)

      fct_sweetAlertSpinner("Processing cohorts")

      webApiUrl <- r_databaseConnection$atlasConfig$webapiurl

      selectedCohortIds <- r$atlasCohortsTable |>
        dplyr::slice(r$selectedIndex) |>
        dplyr::pull(id)

      cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
        baseUrl = webApiUrl,
        cohortIds = selectedCohortIds
      )

      r_cohortDefinitionSetToAdd$databaseName <- input$selectDatabases_pickerInput
      r_cohortDefinitionSetToAdd$cohortDefinitionSet <- cohortDefinitionSet

      ParallelLogger::logInfo("[Import from Atlas-", filterCohortsRegex,"] Importing cohorts: ", r_cohortDefinitionSetToAdd$cohortDefinitionSet$cohortName,
                              " with ids: ", r_cohortDefinitionSetToAdd$cohortDefinitionSet$cohortId,
                              " to database", input$selectDatabases_pickerInput)

      fct_removeSweetAlertSpinner()
    })

    #
    # evaluate the cohorts to append; if accepted increase output to trigger closing actions
    #
    rf_append_accepted_counter <- mod_fct_appendCohort_server("import_atlas", r_databaseConnection, r_cohortDefinitionSetToAdd )

    # close and reset
    shiny::observeEvent(rf_append_accepted_counter(), {
      r_cohortDefinitionSetToAdd$cohortDefinitionSet <- NULL
      reactable::updateReactable("cohorts_reactable", selected = NA, session = session )
    })

  })
}
