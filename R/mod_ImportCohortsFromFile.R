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
    shiny::actionButton(ns("importModal_button"), "Import Data"),
    htmltools::hr(),
    reactable::reactableOutput(ns("cohorts_reactable")),
    htmltools::hr(),
    shiny::actionButton(ns("import_actionButton"), "Import Selected"),
    # TEMP add dummy tooltip, otherwise the import modal stops working on the second import
    # dirty hack ever, but i dont see other solution
    .tippyText('','')
    # END TEMP
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

    regex_person_id <- shiny::reactive(r_databaseConnection$atlasConfig$regexPersonSourceValue)

    #
    # reactive variables
    #
    r_importedData <- shiny::reactiveValues(
      data = NULL,
      name = NULL
    )

    r_dataToMap <- shiny::reactiveValues(
      data = NULL,
      name = NULL
    )

    r_cohortData <- shiny::reactiveValues(
      data = NULL,
      validated = FALSE
    )

    r_cohortDefinitionSetToAdd <- shiny::reactiveValues(
      cohortDefinitionSet = NULL
    )

    #
    # open the import modal
    #
    shiny::observeEvent(input$importModal_button, {
      import_modal(id = ns("importFile_modal"))

       
    })
      import_server("importFile_modal", r_importedData)

    #
    # Evaluate the imported table
    # We try to automatically detect the type of data and convert it to a cohortData format
    # - if it has 1 column check if it is a list of person_ids
    # - if it has 4 columns or more check if it is cohortData
    # if this fails we open the mapping dialog
    #
    shiny::observeEvent(r_importedData$data, {
      shiny::req(r_importedData$data)
      importedTable <- r_importedData$data |> tibble::as_tibble()
      fileName <- r_importedData$name

      cohortDataUploaded <- NULL
      # if it only has one column it is probably a list of person_ids
      if (ncol(importedTable) == 1) {
        # if the column name matches the regex_person_id, it has not header, so we add one
        if (stringr::str_detect(colnames(importedTable)[1], regex_person_id())) {
          importedTable <- dplyr::bind_rows(
            tibble::tibble(person_source_value = colnames(importedTable)[1]),
            importedTable |> dplyr::rename(person_source_value = 1)
          )
        } else {
          importedTable <- importedTable |> dplyr::rename(person_source_value = 1)
        }

        shinyWidgets::sendSweetAlert(
          session = session,
          type = "info",
          title = "Imported data",
          text = shiny::HTML(
            "<small>Data has been detected as a list of person_ids. ", "<ul>",
            "<li> This will create a cohort with all persons in the list.</li>",
            "<li> Cohort name will be set to the name of the imported file.</li>",
            "<li> Cohort start and end date will be set to the first and last observation date of the person.</li>",
            "</ul></small>"
          ),
          html = TRUE
        )

        cohortDataUploaded <- importedTable |>
          dplyr::transmute(
            cohort_name = as.character(fileName),
            person_source_value = as.character(person_source_value),
            cohort_start_date = as.Date(NA),
            cohort_end_date = as.Date(NA)
          )
      }

      # if it has 4 columns or more check if it is cohortData
      if (ncol(importedTable) >= 4) {
        colnames(importedTable) <- tolower(colnames(importedTable))
        # check if it is cohortData
        if (all(c("cohort_name", "person_source_value", "cohort_start_date", "cohort_end_date") %in% colnames(importedTable))) {
          cohortDataUploaded <- importedTable |>
            dplyr::transmute(
              cohort_name = as.character(cohort_name),
              person_source_value = as.character(person_source_value),
              cohort_start_date = as.Date(cohort_start_date),
              cohort_end_date = as.Date(cohort_end_date)
            )

          isCohortData <- HadesExtras::checkCohortData(cohortDataUploaded)
          if (is.character(isCohortData)) {
            cohortDataUploaded <- NULL
          }
        }
        # TEMP: Check if FINNGENID column exists and rename to person_source_value
        if (all(c("cohort_name", "finngenid", "cohort_start_date", "cohort_end_date") %in% colnames(importedTable))) {
          cohortDataUploaded <- importedTable |>
            dplyr::transmute(
              cohort_name = as.character(cohort_name),
              person_source_value = as.character(finngenid),
              cohort_start_date = as.Date(cohort_start_date),
              cohort_end_date = as.Date(cohort_end_date)
            )

          isCohortData <- HadesExtras::checkCohortData(cohortDataUploaded)
          if (is.character(isCohortData)) {
            cohortDataUploaded <- NULL
          }
        }
        # END TEMP
      }

      # if all the above automatic checks succeed set to cohortDataUploaded, if not open the mapping dialog
      if (!is.null(cohortDataUploaded)) {
        r_cohortData$data <- cohortDataUploaded
      } else {
        r_dataToMap$data <- importedTable
        r_dataToMap$name <- fileName
        mapping_modal(id = ns("mappingModal"))
      }
      r_importedData$data <- NULL
      r_importedData$name <- NULL
    })

    mapping_server("mappingModal", r_dataToMap, r_cohortData, regex_person_id)

    shiny::observe({
      shiny::req(r_cohortData$data)
      cohortData <- r_cohortData$data

      check <- HadesExtras::checkCohortData(cohortData)
      if (is.character(check)) {
        shinyWidgets::sendSweetAlert(
          session = session,
          type = "error",
          title = "Unexpected error importing data",
          text = check
        )
        r_cohortData$validated <- FALSE
      } else {
        r_cohortData$validated <- TRUE
      }

      r_dataToMap$data <- NULL
      r_dataToMap$name <- NULL
    })

    #
    # updates output$cohorts_reactable with r$cohortDefinitionSetImported
    #
    output$cohorts_reactable <- reactable::renderReactable({
      shiny::req(r_cohortData$data)
      shiny::req(r_cohortData$validated == TRUE)

      .reactatable_cohortData(r_cohortData$data)
    })

    # reactive function to get selected values
    r_selectedIndex <- shiny::reactive(reactable::getReactableState("cohorts_reactable", "selected", session))

    #
    # button import selected: checks selected cohorts
    #
    shiny::observe({
      shinyjs::toggleState("import_actionButton", condition = !is.null(r_selectedIndex()))
    })
    
    shiny::observeEvent(input$import_actionButton, {
      shiny::req(r_selectedIndex())
      shiny::req(r_cohortData$data)

      fct_sweetAlertSpinner("Importing cohorts")

      selectedCohortNames <- r_cohortData$data |>
        dplyr::distinct(cohort_name) |>
        dplyr::arrange(cohort_name) |>
        dplyr::slice(r_selectedIndex()) |>
        dplyr::pull(cohort_name)

      selectedCohortData <- r_cohortData$data |>
        dplyr::filter(cohort_name %in% selectedCohortNames)

      # calculate new cohorIds
      numberNewCohorts <- selectedCohortData |>
        dplyr::distinct(cohort_name) |>
        nrow()
      unusedCohortIds <- setdiff(1:1000, r_databaseConnection$cohortTableHandler$cohortDefinitionSet$cohortId) |> head(numberNewCohorts)

      ## copy selected to
      r_cohortDefinitionSetToAdd$cohortDefinitionSet <- HadesExtras::cohortDataToCohortDefinitionSet(
        cohortData = selectedCohortData,
        newCohortIds = unusedCohortIds,
        skipCohortDataCheck = TRUE
      )

      ParallelLogger::logInfo(
        "[Import File] Importing cohorts: ", r_cohortDefinitionSetToAdd$cohortDefinitionSet$cohortName,
        " with ids: ", r_cohortDefinitionSetToAdd$cohortDefinitionSet$cohortId
      )

      fct_removeSweetAlertSpinner()
    })

    #
    # evaluate the cohorts to append; if accepted increase output to trigger closing actions
    #
    rf_append_accepted_counter <- mod_fct_appendCohort_server("impor_file", r_databaseConnection, r_cohortDefinitionSetToAdd)

    # # close and reset
    shiny::observeEvent(rf_append_accepted_counter(), { 
      r_dataToMap$data <- NULL
      r_dataToMap$name <- NULL
      r_cohortData$data <- NULL
      r_cohortData$validated <- FALSE
      r_cohortDefinitionSetToAdd$cohortDefinitionSet <- NULL
      reactable::updateReactable("cohorts_reactable", selected = NA, session = session)
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
    dplyr::arrange(cohort_name) |>
    dplyr::select(cohort_name, n_str) |>
    reactable::reactable(
      columns = list(
        cohort_name = reactable::colDef(
          name = "Cohort Name"
        ),
        n_str = reactable::colDef(
          name = "N Subjects (N Entries)"
        )
      ),
      selection = "multiple",
      onClick = "select"
    )

  return(table)
}



.tippyText <- function(text, tooltip) {
  tippy::tippy(
    text = text,
    tooltip = paste0("<div style='text-align: left; font-size:16px;'>", tooltip, "<div>"),
    allowHTML = TRUE,
    theme = "light",
    arrow = TRUE
  )
}