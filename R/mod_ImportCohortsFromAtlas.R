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
    reactable::reactableOutput(ns("cohorts_reactable")) |> ui_load_spinner(),
    htmltools::hr(),

    shiny::actionButton(ns("import_actionButton"), "Import Selected")
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
mod_importCohortsFromAtlas_server <- function(id, r_databaseConnection, filterCohortsRegex = "*") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      selectedIndex = NULL,
      atlasCohortsTable = NULL,
      shortnameEdits = NULL
    )

    r_cohortDefinitionSetToAdd <- shiny::reactiveValues(
      cohortDefinitionSet = NULL
    )

    #
    # Get cohorts from Atlas into r$atlasCohortsTable
    #
    shiny::observe({
      shiny::req(r_databaseConnection$cohortTableHandler)
      input$refreshDatabases_actionButton

      webApiUrl <- r_databaseConnection$atlasConfig$webapiurl

      r$atlasCohortsTable <- NULL

      ParallelLogger::logInfo("[Import from Atlas-", filterCohortsRegex, "] Load from url: ", webApiUrl)

      atlasCohortsTable <- NULL
      tryCatch(
        {
          atlasCohortsTable <- ROhdsiWebApi::getCohortDefinitionsMetaData(webApiUrl) |>
            dplyr::filter(grepl(filterCohortsRegex, name)) |>
            dplyr::arrange(dplyr::desc(id)) |>
            dplyr::select(id, name, description)

          atlasCohortsTable$short_name <- ""

        },
        error = function(e) {
          atlasCohortsTable <<- paste("Error connecting to Atlas. Check that Atlas is working.")
        }
      )

      r$atlasCohortsTable <- atlasCohortsTable
    })

    #
    # render cohorts_reactable
    #
    output$cohorts_reactable <- reactable::renderReactable({
      shiny::req(r$atlasCohortsTable, cancelOutput = TRUE)
      atlasCohortsTable <- r$atlasCohortsTable

      if (is.character(atlasCohortsTable)) {
        ParallelLogger::logWarn("[Import from Atlas-", filterCohortsRegex, "] : ", atlasCohortsTable)
      }
      shiny::validate(shiny::need(!is.character(atlasCohortsTable), atlasCohortsTable))

      edited_idx=NULL
      if(!is.null(r$shortnameEdits)){
        edited_idx <- match(r$shortnameEdits$id, r$atlasCohortsTable$id)
      }

      table <- reactable::reactable(
        atlasCohortsTable,
        columns = list(
          id = reactable::colDef(name = "Cohort ID"),
          name = reactable::colDef(name = "Cohort Name"),
          description = reactable::colDef(name = "Description"),
          short_name = reactable::colDef(name = "Short Name")
        ),
        selection = "multiple",
        onClick = "select",
        searchable = TRUE,
        defaultSelected = edited_idx
      )

      table
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
      #shinyjs::toggleState("editShortNamesBtn", condition = !is.null(r$selectedIndex))

    })

    shiny::observeEvent(input$import_actionButton, {

      shiny::req(r$atlasCohortsTable)
      shiny::req(r$selectedIndex)

      shiny::showModal(shiny::modalDialog(
        title = "Edit Short Names",
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("saveShortNames"), "Import")
        ),
        shiny::uiOutput(ns("shortNameEditUI"))
      ))
    })


    # Render text inputs for selected rows in modal
    output$shortNameEditUI <- renderUI({
      req(r$selectedIndex)

      df_selected <- r$atlasCohortsTable |> dplyr::slice(r$selectedIndex)

      selected_table <- reactable::reactable(
        df_selected,
        columns = list(
          id = reactable::colDef(name = "Cohort ID"),
          short_name = reactable::colDef(
            name = "Short name (defaults to first4 & last4 characters or C#)",
            width = 200,
            cell = function(value, index) {
              # plain HTML input
              rowid <- df_selected$id[index]
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


    #
    # Import and Save short name edits
    #

    observeEvent(input$saveShortNames, {
      shiny::req(r$atlasCohortsTable)
      shiny::req(r$selectedIndex)

      userInputVec <- input$short_name_data

      df_shortnames <- data.frame(
        id = as.numeric(userInputVec[names(userInputVec) == "id"]),
        short_name = userInputVec[names(userInputVec) == "short_name"],
        stringsAsFactors = FALSE
      )
      r$shortnameEdits <-  df_shortnames

      r$atlasCohortsTable <- r$atlasCohortsTable |>
        dplyr::rows_update(
          r$shortnameEdits,
          by = "id"
        )

      removeModal()

      fct_sweetAlertSpinner("Processing cohorts")

      webApiUrl <- r_databaseConnection$atlasConfig$webapiurl
      sourceKey <- r_databaseConnection$atlasConfig$sourcekey
      resultsSchema <- r_databaseConnection$atlasConfig$resultsshchema

      selectedCohortIds <- r$atlasCohortsTable |>
        dplyr::slice(r$selectedIndex) |>
        dplyr::pull(id)

      # Create cohort definition set
      cohortDefinitionSet <- NULL

      if (is.null(sourceKey) || sourceKey == "" || is.null(resultsSchema) || resultsSchema == "") {
        ParallelLogger::logInfo("[Import from Atlas-", filterCohortsRegex, "] import cohortDefinitionSet from Atlas")

        cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
          baseUrl = webApiUrl,
          cohortIds = selectedCohortIds
        )

        # calculate new cohorIds
        numberNewCohorts <- length(selectedCohortIds)
        unusedCohortIds <- setdiff(1:1000, r_databaseConnection$cohortTableHandler$cohortDefinitionSet$cohortId) |> head(numberNewCohorts)

        cohortDefinitionSet <- cohortDefinitionSet |>
          dplyr::mutate(
            cohortId = unusedCohortIds
          )
      } else {
        ParallelLogger::logInfo("[Import from Atlas-", filterCohortsRegex, "] import cohorts build in Atlas")


        validCohortIds <- NULL
        latestGenerationTimestamps <- as.POSIXct(character(0))
        for (cohortId in selectedCohortIds) {
          cohortName <- r$atlasCohortsTable |>
            dplyr::filter(id == cohortId) |>
            dplyr::pull(name)

          cohortDef <- ROhdsiWebApi::getCohortDefinition(
            cohortId = cohortId,
            baseUrl = webApiUrl
          )

          # get the timestamp of the cohort definition
          cohortDefinitionTimestamp <- cohortDef$createdDate
          if ("modifiedDate" %in% names(cohortDef)) {
            cohortDefinitionTimestamp <- cohortDef$modifiedDate
          }

          # get the timestamp of the latest generation
          cohortGenerationTimestamp <- .getCohortGenerationTimestamp(cohortId, webApiUrl, sourceKey)

          # cohortStatus
          cohortStatus <- "NOT GENERATED"
          if (!is.null(cohortGenerationTimestamp)) {
            cohortStatus <- "COMPLETE"
            if (cohortGenerationTimestamp < cohortDefinitionTimestamp) {
              cohortStatus <- "OUTDATED"
            }
          }
          ParallelLogger::logInfo("[Import from Atlas-", filterCohortsRegex, "] Cohort ", cohortId, " status: ", cohortStatus)

          if (cohortStatus == "NOT GENERATED" || cohortStatus == "OUTDATED") {
            ParallelLogger::logInfo("[Import from Atlas-", filterCohortsRegex, "] Generating cohort ", cohortId, " in Atlas")

            ROhdsiWebApi::invokeCohortGeneration(
              cohortId = cohortId,
              baseUrl = webApiUrl,
              sourceKey = sourceKey
            )

            shiny::showModal(
              shiny::modalDialog(
                title = "Generating cohort in Atlas",
                paste0("Atlas' cohort definition for ", cohortName, " is ", cohortStatus, ". It will be generated in Atlas, this may take a few minutes."),
                footer = NULL,
                easyClose = FALSE,
                size = "s"
              )
            )

            # wait for generation to be complete
            counter <- 0
            cohortGenerationTimestamp <- ifelse(is.null(cohortGenerationTimestamp), 0, cohortGenerationTimestamp)
            while (TRUE) {
              newCohortGenerationTimestamp <- .getCohortGenerationTimestamp(cohortId, webApiUrl, sourceKey)

              if (!is.null(newCohortGenerationTimestamp) && newCohortGenerationTimestamp > cohortGenerationTimestamp) {
                break
              }

              Sys.sleep(1)
              counter <- counter + 1
              if (counter > 60 * 5) {
                ParallelLogger::logInfo("[Import from Atlas-", filterCohortsRegex, "] Cohort ", cohortId, " generation aborted")
                shinyWidgets::sendSweetAlert(
                  title = "Generating cohort in Atlas",
                  text = paste0("The time limit for generating the cohort has been reached: ", cohortName, "<br>Please try again later"),
                  type = "error",
                  html = TRUE,
                  showCloseButton = TRUE
                )
                break
              }
            }
            cohortGenerationTimestamp <- newCohortGenerationTimestamp
          }

          shiny::removeModal()

          ParallelLogger::logInfo("[Import from Atlas-", filterCohortsRegex, "] Cohort ", cohortId, " generation complete")

          # get the latest status of the cohort
          cohortGenerationInfo <- ROhdsiWebApi::getCohortGenerationInformation(
            cohortId = cohortId,
            baseUrl = webApiUrl
          )
          if (nrow(cohortGenerationInfo) != 0) {
            sk <- sourceKey
            cohortStatus <- cohortGenerationInfo |>
              dplyr::filter(sourceKey == sk) |>
              dplyr::arrange(dplyr::desc(startTime)) |>
              dplyr::slice(1) |>
              dplyr::pull(status)
          }

          if (cohortStatus != "COMPLETE") {
            ParallelLogger::logInfo("[Import from Atlas-", filterCohortsRegex, "] Cohort ", cohortId, " generation failed")
            shinyWidgets::sendSweetAlert(
              title = "Generating cohort in Atlas",
              text = paste0("The cohort generation failed: ", cohortName, "<br>Please try again later"),
              type = "error",
              html = TRUE,
              showCloseButton = TRUE
            )
          } else {
            validCohortIds <- c(validCohortIds, cohortId)
            latestGenerationTimestamps <- c(latestGenerationTimestamps, cohortGenerationTimestamp)
          }
        }


        if (length(validCohortIds) != 0) {
          # calculate new cohorIds
          numberNewCohorts <- length(validCohortIds)
          unusedCohortIds <- setdiff(1:1000, r_databaseConnection$cohortTableHandler$cohortDefinitionSet$cohortId) |> head(numberNewCohorts)

          cohortDefinitionTable <- r$atlasCohortsTable |>
            dplyr::filter(id %in% validCohortIds) |>
            dplyr::transmute(
              cohort_definition_id = id,
              cohort_definition_name = name,
              cohort_definition_description = description
            )

          cohortDefinitionSet <- HadesExtras::cohortTableToCohortDefinitionSettings(
            cohortDatabaseSchema = resultsSchema,
            cohortDefinitionTable = cohortDefinitionTable,
            cohortDefinitionIds = validCohortIds,
            newCohortDefinitionIds = unusedCohortIds
          )

          # append the last generation timestamp in the sql column for the incremental mode
          cohortDefinitionSet <- cohortDefinitionSet |>
            dplyr::left_join(
              tibble::tibble(
                cohortId = unusedCohortIds,
                generation_timestamp = latestGenerationTimestamps
              ),
              by = "cohortId"
            ) |>
            dplyr::mutate(
              sql = paste0(
                " -- last generation timestamp: ", generation_timestamp,
                "\n",
                sql
              )
            ) |>
            dplyr::select(-generation_timestamp)
        }
      }

      if (!is.null(cohortDefinitionSet)) {
        r_cohortDefinitionSetToAdd$cohortDefinitionSet <- cohortDefinitionSet

        ParallelLogger::logInfo(
          "[Import from Cohort Table-", filterCohortsRegex, "] Importing cohorts: ", r_cohortDefinitionSetToAdd$cohortDefinitionSet$cohortName,
          " with ids: ", r_cohortDefinitionSetToAdd$cohortDefinitionSet$cohortId
        )
      }


      # Capture the short names from the selected rows to the cohortdefinitionset
      if(!is.null(r$shortnameEdits) && nrow(r$shortnameEdits) > 0){

        r_cohortDefinitionSetToAdd$cohortDefinitionSet <- r_cohortDefinitionSetToAdd$cohortDefinitionSet |>
          dplyr::left_join(r$shortnameEdits |> dplyr::mutate(id = as.integer(id)), by = c("atlasId" = "id")) |>
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



.getCohortGenerationTimestamp <- function(cohortId, webApiUrl, sourceKey) {
  cohortGenerationTimestamp <- NULL

  cohortGenerationInfo <- ROhdsiWebApi::getCohortGenerationInformation(
    cohortId = cohortId,
    baseUrl = webApiUrl
  )
  if (nrow(cohortGenerationInfo) != 0) {
    sk <- sourceKey
    cohortGenerationTimestamp <- cohortGenerationInfo |>
      dplyr::filter(sourceKey == sk) |>
      dplyr::filter(status == "COMPLETE") |>
      dplyr::filter(isValid) |>
      dplyr::filter(!isCanceled) |>
      dplyr::arrange(dplyr::desc(startTime)) |>
      dplyr::slice(1) |>
      dplyr::pull(startTime)

    if (length(cohortGenerationTimestamp) == 0) {
      cohortGenerationTimestamp <- NULL
    }
  }

  return(cohortGenerationTimestamp)
}
