
mod_importCohortsFromAtlas_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    mod_appendCohort_ui(),
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
    shiny::actionButton(ns("refreshDatabases_actionButton"), "Refresh Cohort List"),
    reactable::reactableOutput(ns("cohorts_reactable")) |>  ui_load_spinner(),
    htmltools::hr(),
    shiny::actionButton(ns("import_actionButton"), "Import Selected")
    # toggle import_actionButton

  )
}

mod_importCohortsFromAtlas_server <- function(id, r_connectionHandlers, r_workbench, filterCohortsRegex='*') {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    webApiUrl <- shiny::getShinyOption("cohortOperationsConfig")$webApiUrl

    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      atlasCohortsTable = NULL
    )

    r_toAdd <- shiny::reactiveValues(
      databaseName = NULL,
      cohortDefinitionSetCopy = NULL,
      cohortDefinitionSetCreate = NULL
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
      # trigger update by
      input$refreshDatabases_actionButton
      #
      shiny::req(r_connectionHandlers$databasesHandlers)

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

      if (is.character(atlasCohortsTable)) {
        ParallelLogger::logWarn("[Import from Atlas-", filterCohortsRegex,"] : ", atlasCohortsTable)
      }
      shiny::validate(shiny::need(!is.character(atlasCohortsTable), atlasCohortsTable))

      r$atlasCohortsTable <- atlasCohortsTable

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
    # reactive function to get selected values
    r_selectedIndex <- shiny::reactive(reactable::getReactableState("cohorts_reactable", "selected", session))

    #
    # button import selected: checks selected cohorts
    #
    shiny::observe({
      shinyjs::toggleState("import_actionButton", condition = !is.null(r_selectedIndex()) )
    })

    shiny::observeEvent(input$import_actionButton, {
      shiny::req(r_selectedIndex())

      sweetAlert_spinner("Processing cohorts")
      r_toAdd$databaseName <- input$selectDatabases_pickerInput

      selectedCohortIds <- r$atlasCohortsTable |>
        dplyr::slice(r_selectedIndex()) |>
        dplyr::pull(id)

      # sources from atlas
      sourcesAtlas <- as.data.frame(ROhdsiWebApi::getCdmSources(baseUrl = webApiUrl))
      rownames(sourcesAtlas) <- sourcesAtlas$sourceKey

      # cdm schemas
      cohortTableHandler <- r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandle
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

        # extract general info - for a single cohort
        cohortDef <- ROhdsiWebApi::getCohortDefinition(
          cohortId = cohortId,
          baseUrl = webApiUrl
        )

        # modify or create timestamp
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

        # If ATLAS cohort generation was performed using
        # the same OMOP CDM used by CohortOperation tool - use it.
        # If multiple generations found - use the latest one.
        cohortInfo <- cohortInfo[
          which(cohortInfo$status == 'COMPLETE' &
                  cohortInfo$isValid &
                  !cohortInfo$isCanceled &
                  cohortInfo$startTime > modifyOrCreateTimestamp), ]

        cohortInfo$cdmDatabaseSchema <- sourcesAtlas[cohortInfo$sourceKey, 'cdmDatabaseSchema']
        cohortInfo$vocabDatabaseSchema <- sourcesAtlas[cohortInfo$sourceKey, 'vocabDatabaseSchema']
        cohortInfo$resultsDatabaseSchema <- sourcesAtlas[cohortInfo$sourceKey, 'resultsDatabaseSchema']

        # check which ones of the previous generations were done in the same CDM used in current version of CO
        cohortInfo$cdmSourcesMapped <- unname(sapply(cohortInfo$cdmDatabaseSchema, function(k) length(grep(k, cdmDatabaseSchema))) > 0)
        cohortInfo <- cohortInfo[which(cohortInfo$cdmSourcesMapped), ]

        if (nrow(cohortInfo) >= 1){

          # take the latest generation information
          validCohortGeneration <- cohortInfo[1, ]

          # copy to destination BQ table
          if (validCohortGeneration$personCount > 0){
            validCohortGenerations <- rbind(validCohortGenerations, validCohortGeneration)
          } else {
            ParallelLogger::logInfo("[Import Cohort] no persons in the cohort - skipping", cohortId)
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

        ParallelLogger::logInfo("[Import from Atlas-", filterCohortsRegex,"] Importing cohorts (create new): ",
                                cohortDefinitionSetCreate$cohortName,
                                " with ids: ", cohortDefinitionSetCreate$cohortId,
                                " to database", input$selectDatabases_pickerInput)

      }

      # copy existing cohorts
      if (!is.null(validCohortGenerations)) {

        numberNewCohorts <- length(validCohortGenerations$cohortDefinitionId)
        unusedCohortIdsInTargetTable <- setdiff(1:10000, cohortTableHandler$cohortDefinitionSet$cohortId) |> head(numberNewCohorts)

        atlasCohortDefinitionTable <- r$atlasCohortsTable
        colnames(atlasCohortDefinitionTable) <- paste0(
          "cohort_definition_", colnames(atlasCohortDefinitionTable)
        )

        # fetch resultsDatabaseSchema where atlas cohort generations are saved
        resultsDatabaseSchema <- unique(
          paste0(cdmSchemaProjectId, ".", validCohortGenerations$resultsDatabaseSchema)
        )

        # Copy from atlas results BQ table
        # Use the result schema for atlas cohort generation is taken from combined
        # information about the cohort extracted by ROhdsiWebApi::getCohortDefinition and
        # information about webAPI sources extracted by ROhdsiWebApi::getCdmSources
        # mapped by sourceKey.
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
                                cohortDefinitionSetCopyExisting$cohortName,
                                " with ids: ", cohortDefinitionSetCopyExisting$cohortId,
                                " to database", input$selectDatabases_pickerInput)

        tryCatch({
          .estimate_costs(cohortDefinitionSetCopyExisting$sql, cdmSchemaProjectId)
        }, error=function(e) {
          ParallelLogger::logError("[Import from Atlas]: ", e$message)
        }, warning=function(w) {
          ParallelLogger::logWarn("[Import from Atlas]: ", w$message)
        })

      }

      # merge structures
      cohortDefinitions <- dplyr::bind_rows(
        cohortDefinitionSetCopyExisting, cohortDefinitionSetCreate
      )

      # add list
      r_toAdd$cohortDefinitionSet <- cohortDefinitions
      remove_sweetAlert_spinner()

    })

    #
    # evaluate the cohorts to append; if accepted increase output to trigger closing actions
    #
    r_append_accepted_counter <- mod_appendCohort_server("import_atlas", r_connectionHandlers, r_workbench, r_toAdd )

    # close and reset
    shiny::observeEvent(r_append_accepted_counter(), {
      r_toAdd$cohortDefinitionSetCopy <- NULL
      r_toAdd$cohortDefinitionSetCreate <- NULL
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

  totbytes <- sum(as.numeric(estimations))
  cost <- totbytes * 1e-12 * 6.25

  ParallelLogger::logInfo("[Import from Atlas] Importing existing cohorts cost estimation: ",
                          (cost),"$ (", totbytes * 1e-9, "GB of data)")

}






