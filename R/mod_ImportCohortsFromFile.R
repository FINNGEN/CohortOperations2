
mod_importCohortsFromFile_ui <- function(id) {
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
    shiny::fileInput(ns("uploadedFile"), "Choose a file in cohortData format:",
                     multiple = FALSE,
                     accept = c("text/tsv", "text/tabular-separated-values,text/plain", ".tsv",
                                "text/csv", "text/comma-separated-values,text/plain", ".csv")
    ),
    htmltools::hr(),
    reactable::reactableOutput(ns("cohorts_reactable")), # %>% ui_load_spiner(),
    htmltools::hr(),
    shiny::actionButton(ns("import_actionButton"), "Import Selected")
  )
}

mod_importCohortsFromFile_server <- function(id, r_connectionHandlers, r_workbench) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      uploadedFile = NULL,
      cohortDataUploaded = NULL
    )

    r_toAdd <- shiny::reactiveValues(
      databaseName = NULL,
      cohortDefinitionSet = NULL
    )

    #
    # just pass the info to make it writable
    #
    shiny::observe({
      r$uploadedFile <- input$uploadedFile
    })

    #
    # render selectDatabases_pickerInput
    #
    shiny::observe({
      # r_connectionHandlers$databasesHandlers

      databaseIdNamesList <- fct_getDatabaseIdNamesListFromDatabasesHandlers(r_connectionHandlers$databasesHandlers)

      shinyWidgets::updatePickerInput(
        inputId = "selectDatabases_pickerInput",
        choices = databaseIdNamesList,
        selected = databaseIdNamesList[1])
    })

    #
    # updates r$cohortDefinitionSetImported with uploaded file, or with error
    #
    shiny::observe({
      shiny::req(r$uploadedFile)
      shiny::req(input$selectDatabases_pickerInput)

      ext <- tools::file_ext(r$uploadedFile$datapath)

      # passing error to shiny::validate
      if(ext != "tsv" & ext != "csv"){
        r$cohortDefinitionSetImported <- "ERROR READING FILE:\nI need to know if the file is in .tsv or .csv format, please set the extension acordingly"
        return()
      }

      if(ext == "tsv"){ cohortData <- HadesExtras::readCohortData(r$uploadedFile$datapath, delim = "\t") }
      if(ext == "csv"){ cohortData <- HadesExtras::readCohortData(r$uploadedFile$datapath, delim = ",") }


      isCohortData <- HadesExtras::checkCohortData(cohortData)

      # passing error to shiny::validate
      if(is.character(isCohortData)){
        r$cohortDataUploaded <- paste(c("ERROR READING COHORTDATA FILE:", isCohortData), sep = "\n")
      }else{
        r$cohortDataUploaded <- cohortData
      }

    })

    #
    # updates output$cohorts_reactable with r$cohortDefinitionSetImported
    #
    output$cohorts_reactable <- reactable::renderReactable({
      shiny::req(r$cohortDataUploaded)
      shiny::req(input$selectDatabases_pickerInput)

      shiny::validate(
        shiny::need(!is.character(r$cohortDataUploaded), r$cohortDataUploaded)
      )

      .reactatable_cohortData(r$cohortDataUploaded)

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
      shiny::req(r$cohortDataUploaded)
      shiny::req(input$selectDatabases_pickerInput)

      selectedCohortNames <- r$cohortDataUploaded |>
        dplyr::distinct(cohort_name) |>
        dplyr::arrange(cohort_name) |>
        dplyr::slice(r_selectedIndex()) |>
        dplyr::pull(cohort_name)

      selectedCohortData <- r$cohortDataUploaded |>
        dplyr::filter(cohort_name %in% selectedCohortNames)


      cohortTableHandler <- r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandler

      cohortIds <- cohortTableHandler$getCohortIdAndNames() |>
        dplyr::filter(cohortId < 1000) |> # remove ids created by subsets
        dplyr::pull(cohortId)
      cohortIdOffset <- ifelse(length(cohortIds)==0, 0L, max(cohortIds))


      ## copy selected to
      r_toAdd$databaseName <- input$selectDatabases_pickerInput
      r_toAdd$cohortDefinitionSet <-  HadesExtras::cohortDataToCohortDefinitionSet(
        cohortData = selectedCohortData,
        cohortIdOffset = cohortIdOffset,
        skipCohortDataCheck = TRUE
      )

    })

    #
    # evaluate the cohorts to append; if accepted increase output to trigger closing actions
    #
    r_append_accepted_counter <- mod_appendCohort_server("impor_file", r_connectionHandlers, r_workbench, r_toAdd )

    # close and reset
    shiny::observeEvent(r_append_accepted_counter(), {
      shinyjs::reset("uploadedFile")
      r$uploadedFile <- NULL
      r$cohortDataUploaded <- NULL
      r_toAdd$cohortDefinitionSet <- NULL
      reactable::updateReactable("cohorts_reactable", selected = NA, session = session )
    })

  })
}



.reactatable_cohortData <- function(cohortData) {
  table <- cohortData |>
    dplyr::group_by(cohort_name) |>
    dplyr::summarise(
      n_subjects = length(unique(person_source_value)),
      n_entries = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      n_str = paste0(n_subjects, " (", n_entries, ")")
    ) |>
    dplyr::arrange(cohort_name)|>
    dplyr::select(cohort_name, n_str) |>
    #
    reactable::reactable(
      columns = list(
        cohort_name = reactable::colDef(
          name = "Cohort Name"
        ),
        n_str = reactable::colDef(
          name = "N Subjects (N Entries)"
        )
      ),
      #
      selection = "multiple",
      onClick = "select"
    )

  return(table)

}



















