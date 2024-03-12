
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

mod_importCohortsFromAtlas_server <- function(id, r_connectionHandlers, r_workbench) {
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

      atlasCohortsTable <- NULL
      tryCatch({
        atlasCohortsTable <- ROhdsiWebApi::getCohortDefinitionsMetaData(webApiUrl)
      }, error = function(e) {
        atlasCohortsTable <<- paste("Error connecting to Atlas. Check that Atlas is working.")
      })

      shiny::validate(need(!is.character(atlasCohortsTable), atlasCohortsTable))

      r$atlasCohortsTable <- atlasCohortsTable

      atlasCohortsTable |>
        dplyr::arrange(id) |>
        dplyr::select(cohort_id = id, cohort_name = name)  |>
        reactable::reactable(
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
        dplyr::pull(cohort_id)

      cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
        baseUrl = webApiUrl,
        cohortIds = selectedCohortIds
      )

      r_toAdd$databaseName <- input$selectDatabases_pickerInput
        r_toAdd$cohortDefinitionSet <- cohortDefinitionSet

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
















