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

    # action buttons
    fluidRow(
      column(6, shiny::actionButton(ns("import_actionButton"), "Import Selected")),
      column(6,
             div(
               style = "text-align:right;",
               shiny::actionButton(ns("editShortNamesBtn"), "Enter short names for selected cohorts")
              )
          )
     )
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
      cohortDefinitionTable = NULL,
      shortnameEdits = NULL
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
      ) |> dplyr::arrange(cohort_definition_name)|>
        dplyr::filter(grepl(filterCohortsRegex, cohort_definition_name, perl = TRUE)) |>
        dplyr::mutate(cohort_definition_name = stringr::str_remove(cohort_definition_name, filterCohortsRegexRemove))

      cohortDefinitionTable$short_name <- ""
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

      edited_idx=NULL
      if(!is.null(r$shortnameEdits)){
        edited_idx <- match(r$shortnameEdits$id, r$cohortDefinitionTable$cohort_definition_id)
      }

      columns <- list(
        cohort_definition_id = reactable::colDef(show = FALSE),
        cohort_definition_name = reactable::colDef(name = paste0(filterCohortsName, " Name")),
        cohort_definition_description = reactable::colDef(name = paste0(filterCohortsName, " Description"))
      )


      cohortDefinitionTable |>
        reactable::reactable(
          columns = columns,
          selection = "multiple",
          onClick = "select",
          searchable = TRUE,
          defaultSelected = edited_idx
        )
    })
    # Copy to reactive variable, (better than reactive value for testing)
    shiny::observe({
      selectedIndex <- reactable::getReactableState("cohorts_reactable", "selected", session)
      r$selectedIndex <- selectedIndex
    })

    #
    # button edit selected: allows bulk entering of short names for selected cohorts
    #
    observeEvent(input$editShortNamesBtn, {
      shiny::req(r$cohortDefinitionTable)
      shiny::req(r$selectedIndex)

      shiny::showModal(shiny::modalDialog(
        title = "Edit Short Names",
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          shiny::actionButton(ns("saveShortNames"), "Save"),
          shiny::modalButton("Cancel")
        ),
        shiny::uiOutput(ns("shortNameEditUI"))
      ))
    })

    # Render text inputs for selected rows in modal
    output$shortNameEditUI <- renderUI({
      req(r$selectedIndex)

      df_selected <- r$cohortDefinitionTable |> dplyr::slice(r$selectedIndex)

      selected_table <- reactable::reactable(
        df_selected,
        columns = list(
          cohort_definition_id = reactable::colDef(name = "Cohort definition ID"),
          short_name = reactable::colDef(
            name = "Short name (defaults to C#, e.g C1)",
            width = 200,
            cell = function(value, index) {
              # plain HTML input
              rowid <- df_selected$cohort_definition_id[index]
              htmltools::tags$input(
                type = "text",
                value = value,
                `data-rowid` = rowid,
                style = "width:100%; font-size:12px; padding:2px;"
              )
            }
          )
        ),
        # bordered = TRUE,
        compact = TRUE
      )

      # JS: capture all short_name edits in real time and push to Shiny
      selected_table <- htmlwidgets::onRender(
        selected_table,
        sprintf("
          function(el, x) {
            var shortMap = {};

            // Listen for input changes in the table
            el.addEventListener('input', function(e) {
              var t = e.target;
              if (t && t.dataset && t.dataset.rowid) {
                shortMap[t.dataset.rowid] = t.value;
                Shiny.setInputValue('%s',
                  Object.values(shortMap).map((v, i) => ({id: Object.keys(shortMap)[i], short_name: v})),
                  {priority:'event'});
              }
            });
          }
        ", ns("short_name_data"))
      )

      selected_table
    })

    # Save short name edits
    observeEvent(input$saveShortNames, {
      req(r$selectedIndex)

      userInputVec <- input$short_name_data

      df_shortnames <- data.frame(
        id = as.numeric(userInputVec[names(userInputVec) == "id"]),
        short_name = userInputVec[names(userInputVec) == "short_name"],
        stringsAsFactors = FALSE
      )
      r$shortnameEdits <-  df_shortnames

      r$cohortDefinitionTable <- r$cohortDefinitionTable %>%
        dplyr::rows_update(
          r$shortnameEdits |> dplyr::rename(cohort_definition_id = id),
          by = c("cohort_definition_id")
        )

      removeModal()


    })



    #
    # button import selected: checks selected cohorts
    #
    observe({
      shinyjs::toggleState("import_actionButton", condition = !is.null(r$selectedIndex))
      shinyjs::toggleState("editShortNamesBtn", condition = !is.null(r$selectedIndex))

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

      cohortDefinitionSet <- cohortDefinitionSet |>
        dplyr::mutate(
          cohort_definition_id = selectedCohortIds
        )

      r_cohortDefinitionSetToAdd$cohortDefinitionSet <- cohortDefinitionSet

      ParallelLogger::logInfo(
        "[Import from Cohort Table-", filterCohortsRegex, "] Importing cohorts: ", r_cohortDefinitionSetToAdd$cohortDefinitionSet$cohortName,
        " with ids: ", r_cohortDefinitionSetToAdd$cohortDefinitionSet$cohortId
      )

      # Capture the short names from the selected rows to the cohortdefinitionset
      if(!is.null(r$shortnameEdits)){

        r_cohortDefinitionSetToAdd$cohortDefinitionSet <- r_cohortDefinitionSetToAdd$cohortDefinitionSet |>
          dplyr::left_join(r$shortnameEdits |> dplyr::mutate(id = as.integer(id)), by = c("cohort_definition_id" = "id")) |>
          dplyr::rename(shortName = short_name)

        r$shortnameEdits <- NULL

      }


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
