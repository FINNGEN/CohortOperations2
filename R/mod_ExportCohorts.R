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


mod_exportsCohorts_server <- function(id, r_connectionHandlers, r_workbench) {

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
    shiny::observeEvent(r_workbench$cohortsSummaryDatabases, {

      cohortIdAndNamesList <- list()
      for(databaseId in unique(r_workbench$cohortsSummaryDatabases$databaseId)){
        cohortIdAndNames <- r_workbench$cohortsSummaryDatabases |> dplyr::filter(databaseId == !!databaseId)
        cohortIdAndNamesList[databaseId] <- list(as.list(setNames(paste0(databaseId, "@", cohortIdAndNames$cohortId), paste0(cohortIdAndNames$shortName,  " (" , cohortIdAndNames$cohortName, ")"))))
     }
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "selectCohorts_pickerInput",
        choices = cohortIdAndNamesList,
        selected = NULL
      )
    })

    #
    # prepare list of selected cohorts and filename
    #
    shiny::observeEvent(input$selectCohorts_pickerInput, {
      shiny::req(r_workbench$cohortsSummaryDatabases)
      shiny::req(input$selectCohorts_pickerInput)

      selectedCohortsInfo <- r_workbench$cohortsSummaryDatabases  |>
        dplyr::filter(paste0(databaseId, "@",cohortId) %in% input$selectCohorts_pickerInput) |>
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

        sweetAlert_spinner("Preparing cohort for download")

        selectedCohortsInfo <- r$selectedCohortsInfo

        result <- tibble::tibble()
        for (databaseId in unique(selectedCohortsInfo$databaseId)){
          cohortIds <- selectedCohortsInfo  |> dplyr::filter(databaseId == {{databaseId}}) |> dplyr::pull(cohortId)
          cohortNames <- selectedCohortsInfo  |> dplyr::filter(databaseId == {{databaseId}}) |> dplyr::pull(cohortName)
          cohortTableHandler <- r_connectionHandlers$databasesHandlers[[databaseId]]$cohortTableHandler

          cohortData <- HadesExtras::getCohortDataFromCohortTable(
            connection = cohortTableHandler$connectionHandler$getConnection(),
            cdmDatabaseSchema = cohortTableHandler$cdmDatabaseSchema,
            cohortDatabaseSchema = cohortTableHandler$cohortDatabaseSchema,
            cohortTable = cohortTableHandler$cohortTableNames$cohortTable,
            cohortNameIds = tibble::data_frame(cohortId=cohortIds, cohortName=cohortNames)
          )

          cohortData <- cohortData  |>
            tibble::add_column( database_id = rep(databaseId, nrow(cohortData)), .before = 1)

            # TEMP to compatible with CO1
          if (input$co1Compatible_checkbox){
            cohortData <- cohortData  |>
            dplyr::transmute(
              COHORT_SOURCE  = database_id,
              COHORT_NAME = cohort_name,
              FINNGENID = person_source_value,
              COHORT_START_DATE = cohort_start_date,
              COHORT_END_DATE = cohort_end_date,
            )
          }

          result <- rbind(result, cohortData)

        }

        writeErrorMessage <- ""
        tryCatch({
          write.table(result, filename, row.names=FALSE, sep="\t", quote = F, append = F)
        }, error=function(e) {
          writeErrorMessage <<- e$message
          ParallelLogger::logError("[Export Cohorts] write table: ", e$message)
        }, warning=function(w) {
          writeErrorMessage <<-  w$message
          ParallelLogger::logWarn("[Export Cohorts] write table: ", w$message)
        })

        r$writeErrorMessage <- writeErrorMessage

        remove_sweetAlert_spinner()

      }
    )

    shiny::observe({
      shiny::req(r$writeErrorMessage)
      browser()
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
      r$writeErrorMessage <- NULL
    })


  })


}


.format_str <- function(x){
  tolower(stringr::str_replace_all(x, "[[:punct:]]", "")  |>  stringr::str_replace_all( " ", "_"))
}
