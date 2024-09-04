#' operateCohorts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS fileInput actionButton
#' @importFrom htmltools tagList hr
#' @importFrom shinyjs useShinyjs
#' @importFrom reactable reactableOutput
mod_exportsCohorts_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyjs::useShinyjs(),
    shiny::tags$h4("Cohort"),
    shiny::tags$h5("Select cohort:"),
    shinyWidgets::pickerInput(
      inputId = ns("selectCohorts_pickerInput"),
      width = "600px",
      label = NULL,
      choices = NULL,
      selected = NULL,
      options = list(
        `actions-box` = TRUE),
      multiple = TRUE),
    # binary selection "make ourput compatible with CO1"
    shiny::checkboxInput(
      inputId = ns("co1Compatible_checkbox"),
      label = "Make output colums compatible with CO1",
      value = TRUE),
    htmltools::hr(),
    shiny::downloadButton(ns("downloadData_downloadButton"), "Export")
  )
}


mod_exportsCohorts_server <- function(id, r_connectionHandler) {

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      selectedCohortsInfo = NULL,
      filename = NULL,
      writeErrorMessage = NULL
    )

    #
    # update selectCohorts_pickerInput with database/cohort names
    #
    shiny::observe({
      shiny::req(r_connectionHandler$cohortTableHandler)
      shiny::req(r_connectionHandler$hasChangeCounter)

      cohortIdAndNames <- r_connectionHandler$cohortTableHandler$getCohortIdAndNames()
      cohortIdAndNamesList <- list()
      if(nrow(cohortIdAndNames) != 0){
        cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, paste(cohortIdAndNames$shortName, "("  , cohortIdAndNames$cohortName, ")")))
      }

      shinyWidgets::updatePickerInput(
        inputId = "selectCohorts_pickerInput",
        choices = cohortIdAndNamesList,
        selected = character(0)
      )
    })

    #
    # prepare list of selected cohorts and filename
    #
    shiny::observeEvent(input$selectCohorts_pickerInput, {

      selectedCohortsInfo <- r_connectionHandler$cohortTableHandler$getCohortsSummary()  |>
        dplyr::filter(cohortId %in% input$selectCohorts_pickerInput) |>
        dplyr::select(databaseId, cohortId, cohortName, databaseName)

      name <- paste0(paste0(.format_str(selectedCohortsInfo$databaseId), "_", .format_str(selectedCohortsInfo$cohortName)), collapse = "_")

      r$selectedCohortsInfo <- selectedCohortsInfo
      r$filename <- paste0(name, ".tsv")

    })

    #
    # activate settings if cohorts have been selected
    #
    shiny::observe({
      condition <- shiny::isTruthy(input$selectCohorts_pickerInput)
      shinyjs::toggleState("downloadData_downloadButton", condition = condition )
    })

    output$downloadData_downloadButton <- shiny::downloadHandler(
      filename =  function() {r$filename},
      content = function(filename) {

        fct_sweetAlertSpinner("Preparing cohort for download")

        selectedCohortsInfo <- r$selectedCohortsInfo

        cohortIds <- selectedCohortsInfo  |> dplyr::pull(cohortId)
        cohortNames <- selectedCohortsInfo  |>  dplyr::pull(cohortName)

        cohortData <- HadesExtras::getCohortDataFromCohortTable(
          connection = r_connectionHandler$cohortTableHandler$connectionHandler$getConnection(),
          cdmDatabaseSchema = r_connectionHandler$cohortTableHandler$cdmDatabaseSchema,
          cohortDatabaseSchema = r_connectionHandler$cohortTableHandler$cohortDatabaseSchema,
          cohortTable = r_connectionHandler$cohortTableHandler$cohortTableNames$cohortTable,
          cohortNameIds = tibble::tibble(cohortId=cohortIds, cohortName=cohortNames)
        )

        cohortData <- cohortData  |>
          tibble::add_column( database_id = r_connectionHandler$cohortTableHandler$databaseId, .before = 1)

        # TEMP to compatible with CO1
        if (input$co1Compatible_checkbox){
          cohortData <- cohortData  |>
            dplyr::transmute(
              COHORT_SOURCE  = database_id,
              COHORT_NAME = cohort_name,
              FINNGENID = person_source_value,
              COHORT_START_DATE = cohort_start_date,
              COHORT_END_DATE = cohort_end_date
            )
        }

        writeErrorMessage <- ""
        tryCatch({
          write.table(cohortData, filename, row.names=FALSE, sep="\t", quote = F, append = F)
        }, error=function(e) {
          writeErrorMessage <<- e$message
          ParallelLogger::logError("[Export Cohorts] write table: ", e$message)
        }, warning=function(w) {
          writeErrorMessage <<-  w$message
          ParallelLogger::logWarn("[Export Cohorts] write table: ", w$message)
        })

        r$writeErrorMessage <- writeErrorMessage

        fct_removeSweetAlertSpinner()

      }
    )

    shiny::observe({
      shiny::req(!is.null(r$writeErrorMessage))
      if (r$writeErrorMessage == ""){
        shinyWidgets::show_alert(
          title = "Download completed successfully",
          text = "If you didn't chage the default settings, the file should be in your Downloads folder",
          btn_labels = "OK",
          btn_colors = "#70B6E0",
          width = "550px"
        )
      } else {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error while downloading a file",
          text = r$writeErrorMessage,
          btn_labels = "OK",
          type = "error"
        )
      }

      # reset
      shinyWidgets::updatePickerInput(
        inputId = "selectCohorts_pickerInput",
        selected = character(0)
      )

      r$writeErrorMessage <- NULL

    })
  })


}


.format_str <- function(x){
  tolower(stringr::str_replace_all(x, "[[:punct:]]", "")  |>  stringr::str_replace_all( " ", "_"))
}
