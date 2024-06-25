
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
          dplyr::select(id, name, description, modifiedDate)
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

      selectedCohortIds <- r$atlasCohortsTable |>
        dplyr::slice(r_selectedIndex()) |>
        dplyr::pull(id)

      atlasConfig <- r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$atlasConfig



      for (selectedCohortId in selectedCohortIds) {
        browser()
        # check if cohort has been generated and up to date
        cohortModifiedDate <- r$atlasCohortsTable |>
          dplyr::filter(id == selectedCohortId) |>
          dplyr::pull(modifiedDate)

        cohortGenerationInformation <- ROhdsiWebApi::getCohortGenerationInformation(selectedCohortId, atlasConfig$webapi)|>
          dplyr::filter(sourceKey == atlasConfig$sourceKey)

        cohortIsUpToDate <- cohortModifiedDate <= cohortGenerationInformation$startTime

        if (!isUpToDate) {
          # run it
          ROhdsiWebApi::invokeCohortGeneration(selectedCohortId, atlasConfig$webapi, atlasConfig$sourceKey)
          # wait for completed or failed status
          while (cohortGenerationInformation$status %in% c("PENDING", "RUNNING")) {
            Sys.sleep(5)
            try(
              cohortGenerationInformation <- ROhdsiWebApi::getCohortGenerationInformation(selectedCohortId, atlasConfig$webapi)|>
                dplyr::filter(sourceKey == atlasConfig$sourceKey)
            )
          }
        }

        if (cohortGenerationInformation$status == "FAILED") {
          sweetAlert_error("Cohort generation failed")
          return()
        }

        if (cohortGenerationInformation$status == "COMPLETED") {

          cohortDefinitionTable <-  r$atlasCohortsTable |>
            dplyr::filter(id == selectedCohortId) |>
            dplyr::transmute(
              cohort_definition_id = id,
              cohort_definition_name = name,
              cohort_definition_description = description
            )


          HadesExtras::cohortTableToCohortDefinitionSettings(
            cohortDatabaseSchema = atlasConfig$cohortDatabaseSchema,
            cohortTable = atlasConfig$cohortTable,
            cohortDefinitionTable = cohortDefinitionTable,
            cohortDefinitionIds = selectedCohortId,
            cohortIdOffset = 0L
          ){

            # add cohort to database
            cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
              baseUrl = webApiUrl,
              cohortIds = selectedCohortId
            )

            # add cohort to database
            r_toAdd$databaseName <- input$selectDatabases_pickerInput
            r_toAdd$cohortDefinitionSet <- cohortDefinitionSet

            ParallelLogger::logInfo("[Import from Atlas-", filterCohortsRegex,"] Importing cohorts: ", r_toAdd$cohortDefinitionSet$cohortName,
                                    " with ids: ", r_toAdd$cohortDefinitionSet$cohortId,
                                    " to database", input$selectDatabases_pickerInput)
          }





        }

        cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
          baseUrl = webApiUrl,
          cohortIds = selectedCohortIds
        )

        ROhdsiWebApi::getCohortGenerationInformation(1778211, webApiUrl)





        r_toAdd$databaseName <- input$selectDatabases_pickerInput
        r_toAdd$cohortDefinitionSet <- cohortDefinitionSet

        ParallelLogger::logInfo("[Import from Atlas-", filterCohortsRegex,"] Importing cohorts: ", r_toAdd$cohortDefinitionSet$cohortName,
                                " with ids: ", r_toAdd$cohortDefinitionSet$cohortId,
                                " to database", input$selectDatabases_pickerInput)

        remove_sweetAlert_spinner()
      })

    #
    # evaluate the cohorts to append; if accepted increase output to trigger closing actions
    #
    r_append_accepted_counter <- mod_appendCohort_server("import_atlas", r_connectionHandlers, r_workbench, r_toAdd )

    # close and reset
    shiny::observeEvent(r_append_accepted_counter(), {
      r_toAdd$cohortDefinitionSet <- NULL
      reactable::updateReactable("cohorts_reactable", selected = NA, session = session )
    })

    })


  }
















