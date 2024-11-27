
#' Import Cohorts From File UI Module
#'
#' @description A shiny Module to import cohorts from a file.
#'
#' @param id Module ID
#'
#' @importFrom shiny NS tagList fileInput hr actionButton
#' @importFrom shinyjs useShinyjs
#' @importFrom reactable reactableOutput
#' @importFrom htmltools hr
#'
#' @return A UI definition for the module.
mod_importCohortsFromFile_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    mod_fct_appendCohort_ui(),
    shinyjs::useShinyjs(),
    #
    shiny::fileInput(ns("uploadedFile"), "Choose a file in cohortData format:",
                     multiple = FALSE,
                     accept = c("text/tsv", "text/tabular-separated-values,text/plain", ".tsv",
                                "text/csv", "text/comma-separated-values,text/plain", ".csv")
    ),
    htmltools::hr(),
    reactable::reactableOutput(ns("cohorts_reactable")), # |> ui_load_spiner(),
    htmltools::hr(),
    shiny::actionButton(ns("import_actionButton"), "Import Selected")
  )
}
#' Import Cohorts From File Server Module
#'
#' @description A shiny Module to handle server-side operations for importing cohorts from a file.
#'
#' @param id Module ID
#' @param r_databaseConnection A reactive database connection object
#'
#' @importFrom shiny moduleServer reactiveValues observe observeEvent req renderText validate need
#' @importFrom shinyjs toggleState reset
#' @importFrom shinyWidgets pickerInput
#' @importFrom dplyr group_by summarise mutate arrange select sym distinct filter pull n
#' @importFrom tools file_ext
#' @importFrom stringr str_to_lower
#' @importFrom ParallelLogger logInfo
#' @importFrom HadesExtras readCohortData checkCohortData cohortDataToCohortDefinitionSet
#' @importFrom reactable getReactableState updateReactable
#' @importFrom shinyjs hidden
#' @importFrom shinyWidgets pickerInput
#' @importFrom shiny modalDialog modalButton actionButton fluidRow column textInput verbatimTextOutput
#' @importFrom shinyjs toggle
#'
#' @return A server function for the module.
mod_importCohortsFromFile_server <- function(id, r_databaseConnection) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      uploadedFile = NULL,
      cohortDataUploaded = NULL,
      cohortData = NULL,
      original_colnames = NULL,
      columnNamesOK = FALSE
    )

    r_cohortDefinitionSetToAdd <- shiny::reactiveValues(
      cohortDefinitionSet = NULL
    )

    #
    # just pass the info to make it writable
    #
    shiny::observe({
      r$uploadedFile <- input$uploadedFile
    })


    #
    # updates r$cohortDefinitionSetImported with uploaded file, or with error
    #
    shiny::observe({
      shiny::req(r_databaseConnection$cohortTableHandler)
      shiny::req(r$uploadedFile)

      ParallelLogger::logInfo("[Import File] Opening file: ", r$uploadedFile)

      ext <- tools::file_ext(r$uploadedFile$datapath)

      # passing error to shiny::validate
      if(ext != "tsv" & ext != "csv"){
        r$cohortDefinitionSetImported <- "ERROR READING FILE:\nI need to know if the file is in .tsv or .csv format, please set the extension accordingly"
        return()
      }

      if(ext == "tsv"){ cohortData <- HadesExtras::readCohortData(r$uploadedFile$datapath, delim = "\t") }
      if(ext == "csv"){ cohortData <- HadesExtras::readCohortData(r$uploadedFile$datapath, delim = ",") }

      # we expect lowercase names
      colnames(cohortData) <- stringr::str_to_lower(colnames(cohortData))

      r$original_colnames <- colnames(cohortData)
      r$cohortDataUploaded <- cohortData

      if(!all(c("cohort_name", "person_source_value", "cohort_start_date", "cohort_end_date") %in% colnames(cohortData))){
          ParallelLogger::logInfo("[Import File] File needs column remaping ")
          showModal(assignmentDialog())
      } else {
          importedTable <- importedTable |> dplyr::rename(person_source_value = 1)
        }

      # check if cohort_start_date and cohort_end_date are empty, replace them with NA
      if(cohort_start_date == ""){
        cohort_start_date <- "cohort_start_date"
        cohortData$cohort_start_date <- as.Date(NA)
      }
      if(cohort_end_date == ""){
        cohort_end_date <- "cohort_end_date"
        cohortData$cohort_end_date <- as.Date(NA)
      }

      # change the column names
      cohortData <- cohortData |>
        dplyr::select(dplyr::sym(cohort_name), dplyr::sym(person_source_value), dplyr::sym(cohort_start_date), dplyr::sym(cohort_end_date))
      names(cohortData) <- c("cohort_name", "person_source_value", "cohort_start_date", "cohort_end_date")

      # force date columns to be Dates
      cohortData$cohort_start_date <- as.Date(cohortData$cohort_start_date)
      cohortData$cohort_end_date <- as.Date(cohortData$cohort_end_date)

      # we are done
      r$cohortDataUploaded <- cohortData
      r$columnNamesOK <- TRUE
    })

    #
    # check the cohortData
    #
    shiny::observe({
      shiny::req(r$columnNamesOK)
      shiny::req(r$cohortDataUploaded)

      cohortData <- r$cohortDataUploaded

      r$columnNamesOK <- FALSE

      isCohortData <- HadesExtras::checkCohortData(cohortData)

      # passing error to shiny::validate
      if(is.character(isCohortData)){
        r$cohortData <- paste(c("ERROR READING COHORTDATA FILE:", isCohortData), sep = "\n")
      }else{
        r$cohortData <- cohortData
      }
    })

    #
    # updates output$cohorts_reactable with r$cohortDefinitionSetImported
    #
    output$cohorts_reactable <- reactable::renderReactable({
      shiny::req(r$cohortData)

      shiny::validate(
        shiny::need(!is.character(r$cohortData), r$cohortData)
      )

      .reactatable_cohortData(r$cohortData)

    })

    output$import_help <- shiny::renderText({
      paste(
        "Cohort Operations expects the file in 'cohortData' format.",
        "There should be column names 'cohort_name', 'person_source_value', 'cohort_start_date', 'cohort_end_date',",
        "but there can be others as well.",
        "This dialog will help you to assign the columns to the correct names (does not change your file).",
        "You can select one of the columns to be the cohort name, or give a default name.",
        "The cohort start and end dates can be left empty, ",
        "then they will be set to the minimum and maximum dates in the cohort.",
        sep = "\n"
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

    shiny::observeEvent(input$import_help_button, {
      shinyjs::toggle("import_help")
    })

    shiny::observeEvent(input$import_actionButton, {
      shiny::req(r_selectedIndex())
      shiny::req(r$cohortData)

      fct_sweetAlertSpinner("Importing cohorts")

      selectedCohortNames <- r$cohortData |>
        dplyr::distinct(cohort_name) |>
        dplyr::arrange(cohort_name) |>
        dplyr::slice(r_selectedIndex()) |>
        dplyr::pull(cohort_name)

      selectedCohortData <- r$cohortData |>
        dplyr::filter(cohort_name %in% selectedCohortNames)

      # calculate new cohorIds
      numberNewCohorts <- selectedCohortData |> dplyr::distinct(cohort_name) |> nrow()
      unusedCohortIds <- setdiff(1:1000, r_databaseConnection$cohortTableHandler$cohortDefinitionSet$cohortId) |> head(numberNewCohorts)

      ## copy selected to
      r_cohortDefinitionSetToAdd$cohortDefinitionSet <-  HadesExtras::cohortDataToCohortDefinitionSet(
        cohortData = selectedCohortData,
        newCohortIds = unusedCohortIds,
        skipCohortDataCheck = TRUE
      )

      ParallelLogger::logInfo("[Import File] Importing cohorts: ", r_cohortDefinitionSetToAdd$cohortDefinitionSet$cohortName,
                              " with ids: ", r_cohortDefinitionSetToAdd$cohortDefinitionSet$cohortId)

      fct_removeSweetAlertSpinner()

    })

    #
    # evaluate the cohorts to append; if accepted increase output to trigger closing actions
    #
    rf_append_accepted_counter <- mod_fct_appendCohort_server("impor_file", r_databaseConnection, r_cohortDefinitionSetToAdd )

    # close and reset
    shiny::observeEvent(rf_append_accepted_counter(), {
      shinyjs::reset("uploadedFile")
      r$uploadedFile <- NULL
      r$cohortDataUploaded <- NULL
      r$cohortData <- NULL
      r_cohortDefinitionSetToAdd$cohortDefinitionSet <- NULL
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


















