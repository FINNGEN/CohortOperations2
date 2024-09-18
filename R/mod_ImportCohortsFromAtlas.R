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

      # sources from atlas
      sourcesAtlas <- as.data.frame(ROhdsiWebApi::getCdmSources(baseUrl = webApiUrl))
      rownames(sourcesAtlas) <- sourcesAtlas$sourceKey

      # cdm schemas - selects value for the currently selected picker
      # loads all cohorts from a single OMOP
      cohortTableHandler <- r_databaseConnection$cohortTableHandler
      cdmDatabaseSchema  <- cohortTableHandler$cdmDatabaseSchema
      vocabularyDatabaseSchema  <-  cohortTableHandler$vocabularyDatabaseSchema
      cdmSchemaProjectId <- strsplit(cdmDatabaseSchema, "\\.")[[1]][1]

      # cohort table configs
      cohortTableNames <- cohortTableHandler$cohortTableNames
      cohortDatabaseSchema <- cohortTableHandler$cohortDatabaseSchema

      generateNewCohortIds <- NULL
      validCohortGenerations <- NULL
      cohortDefinitionSetCopyExisting <- NULL
      cohortDefinitionSetCreate <- NULL

      for (cohortId in selectedCohortIds){

        cohortDef <- ROhdsiWebApi::getCohortDefinition(
          cohortId = cohortId,
          baseUrl = webApiUrl
        )

        if ('modifiedDate' %in% names(cohortDef)){
          modifyOrCreateTimestamp <- cohortDef$modifiedDate
        } else {
          modifyOrCreateTimestamp <- cohortDef$createdDate
        }

        # extract info about generations
        cohortInfo <- ROhdsiWebApi::getCohortGenerationInformation(
          cohortId = cohortId,
          baseUrl = webApiUrl
        )

        # add more description
        cohortInfo$description <- cohortDef$description
        cohortInfo$name <- cohortDef$name

        if (nrow(cohortInfo) == 0){
          generateNewCohortIds <- c(generateNewCohortIds, cohortId)
          next
        }

        # Use atlas cohort generation if the generation was built using same
        # OMOP CDM and if multiple generations are found the latest is chosen
        cohortInfo <- cohortInfo[
          which(cohortInfo$status == 'COMPLETE' &
                  cohortInfo$isValid &
                  !cohortInfo$isCanceled &
                  cohortInfo$startTime > modifyOrCreateTimestamp), ]

        cohortInfo$cdmDatabaseSchema <- sourcesAtlas[cohortInfo$sourceKey, 'cdmDatabaseSchema']
        cohortInfo$vocabDatabaseSchema <- sourcesAtlas[cohortInfo$sourceKey, 'vocabDatabaseSchema']
        cohortInfo$resultsDatabaseSchema <- sourcesAtlas[cohortInfo$sourceKey, 'resultsDatabaseSchema']

        # check which ones of the previous generations were done
        # in the same CDM used in current version of CO
        cohortInfo$cdmSourcesMapped <- unname(sapply(
          cohortInfo$cdmDatabaseSchema, function(k)
            length(grep(k, cdmDatabaseSchema))) > 0
        )

        cohortInfo <- cohortInfo[which(cohortInfo$cdmSourcesMapped), ]

        if (nrow(cohortInfo) >= 1){

          # take the latest generation information
          validCohortGeneration <- cohortInfo[1, ]

          # copy to destination BQ table
          if (validCohortGeneration$personCount > 0){
            validCohortGenerations <- rbind(
              validCohortGenerations, validCohortGeneration
            )
          } else {
            ParallelLogger::logInfo(
              "[Import Cohort] no persons in the cohort - skipping", cohortId
            )
          }

        } else {
          # no valid cohorts available - generate new cohort
          generateNewCohortIds <- c(generateNewCohortIds, cohortId)
        }

      }

      # generate new cohorts
      if (length(generateNewCohortIds) > 0){

        cohortDefinitionSetCreate <- ROhdsiWebApi::exportCohortDefinitionSet(
          baseUrl = webApiUrl,
          cohortIds = generateNewCohortIds
        )

        cohortDefinitionSetCreate$cdmDatabaseSchema <- cdmDatabaseSchema
        cohortDefinitionSetCreate$vocabDatabaseSchema <- vocabularyDatabaseSchema
        cohortDefinitionSetCreate$cohortGenerated <- FALSE

        ParallelLogger::logInfo(
          "[Import from Atlas-", filterCohortsRegex,"] Importing cohorts (create new): ",
          paste0(cohortDefinitionSetCreate$cohortName, collapse = ", "),
          " with ids: ", paste0(cohortDefinitionSetCreate$cohortId, collapse = ", "))

      }

      # copy existing cohorts
      if (!is.null(validCohortGenerations)) {

        numberNewCohorts <- length(validCohortGenerations$cohortDefinitionId)
        unusedCohortIdsInTargetTable <- setdiff(1:10000, cohortTableHandler$cohortDefinitionSet$cohortId) |> head(numberNewCohorts)

        atlasCohortDefinitionTable <- r$atlasCohortsTable
        colnames(atlasCohortDefinitionTable) <- paste0(
          "cohort_definition_", colnames(atlasCohortDefinitionTable)
        )

        # schema in which cohort entries are available
        resultsDatabaseSchema <- unique(
          paste0(cdmSchemaProjectId, ".", validCohortGenerations$resultsDatabaseSchema)
        )

        # Copy from atlas results BQ table
        cohortDefinitionSetCopyExisting <- HadesExtras::cohortTableToCohortDefinitionSettings(
          cohortDatabaseSchema = resultsDatabaseSchema,
          cohortDefinitionTable = atlasCohortDefinitionTable,
          cohortDefinitionIds = validCohortGenerations$cohortDefinitionId,
          newCohortDefinitionIds = unusedCohortIdsInTargetTable
        )

        # add atlas ids and schema
        cohortDefinitionSetCopyExisting$atlasId  <- validCohortGenerations$cohortDefinitionId
        cohortDefinitionSetCopyExisting$logicDescription <- validCohortGenerations$description
        cohortDefinitionSetCopyExisting$cdmDatabaseSchema <- paste0(
          cdmSchemaProjectId, ".", validCohortGenerations$cdmDatabaseSchema
        )

        cohortDefinitionSetCopyExisting$vocabDatabaseSchema <- paste0(
          cdmSchemaProjectId, ".", validCohortGenerations$vocabDatabaseSchema
        )

        cohortDefinitionSetCopyExisting$resultsDatabaseSchema <- paste0(
          cdmSchemaProjectId, ".", validCohortGenerations$resultsDatabaseSchema
        )

        cohortDefinitionSetCopyExisting$cohortGenerated <- TRUE

        ParallelLogger::logInfo("[Import from Atlas-",
                                filterCohortsRegex,"] Importing cohorts (copy existing): ",
                                paste0(cohortDefinitionSetCopyExisting$cohortName, collapse = ", "),
                                " with ids: ", paste0(cohortDefinitionSetCopyExisting$cohortId, collapse = ", "))

        tryCatch({
          .estimate_costs(cohortDefinitionSetCopyExisting$sql, cdmSchemaProjectId)
        }, error=function(e) {
          ParallelLogger::logError("[Import from Atlas]: ", e$message)
        }, warning=function(w) {
          ParallelLogger::logWarn("[Import from Atlas]: ", w$message)
        })

      }

      cohortDefinitions <- dplyr::bind_rows(
        cohortDefinitionSetCopyExisting, cohortDefinitionSetCreate
      )

      r_cohortDefinitionSetToAdd$cohortDefinitionSet <- cohortDefinitions

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


.estimate_costs <- function(queries, projectId){

  options(gargle_oauth_cache = FALSE)
  bigrquery::bq_auth(scopes = "https://www.googleapis.com/auth/bigquery")

  estimations <- c()
  for (i in 1:length(queries)){
    query <- queries[i]

    sp <- strsplit(queries, split = '\n')[[1]]
    rowid <- grep("SELECT", sp)
    sql <- paste0(sp[rowid:length(sp)], collapse = "\n")

    e <- bigrquery::bq_perform_query_dry_run(sql, projectId)
    estimations <- c(estimations, e)
  }

  totbytes <- round(sum(as.numeric(estimations)))

  ParallelLogger::logInfo("[Import from Atlas] Importing existing cohorts billing estimation: ",
                          totbytes * 1e-9," GB (", totbytes, "b)")

}


