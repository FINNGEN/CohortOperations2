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
    shiny::tags$h4("Cohort"),
    shiny::tags$h5("Select cohort:"),
    shinyWidgets::pickerInput(
      inputId = ns("selectCohorts_pickerInput"),
      label = NULL,
      choices = NULL,
      selected = NULL,
      multiple = FALSE),
    htmltools::hr(),
    shiny::downloadButton(ns("downloadData"), "Export")
  )
}

format_str <- function(x){
  tolower(stringr::str_replace_all(x, "[[:punct:]]", "") %>% stringr::str_replace_all(.,  " ", "_"))
}

mod_exportsCohorts_server <- function(id, r_connectionHandlers, r_workbench) {

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      cohortDefinitionSet = NULL,
      operationStringError = NULL
    )

    r_toAdd <- shiny::reactiveValues(
      databaseName = NULL,
      cohortDefinitionSet = NULL
    )

    r_data <- shiny::reactiveValues(
      databaseId = NULL,
      cohortId = NULL,
      filename = NULL,
      cohortName = NULL
    )

    r_status <- shiny::reactiveValues(
      success = NULL
    )

    #
    # render selectCohorts_pickerInput with database/cohort names
    #
    shiny::observeEvent(r_workbench$cohortsSummaryDatabases, {

      cohortIdAndNamesList <- list()
      for(databaseId in unique(r_workbench$cohortsSummaryDatabases$databaseId)){
        cohortIdAndNames <- r_workbench$cohortsSummaryDatabases |> dplyr::filter(databaseId == !!databaseId) |> dplyr::select(cohortId, shortName)
        cohortIdAndNamesList[databaseId] <- list(as.list(setNames(paste0(databaseId, "@", cohortIdAndNames$cohortId), cohortIdAndNames$shortName)))
      }
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "selectCohorts_pickerInput",
        choices = cohortIdAndNamesList,
        selected = NULL
      )
    })

    shiny::observeEvent(input$selectCohorts_pickerInput, {
      shiny::req(r_workbench$cohortsSummaryDatabases)
      shiny::req(input$selectCohorts_pickerInput)

      selected <- input$selectCohorts_pickerInput
      databaseId <- strsplit(selected, '@')[[1]][1]
      cohortId <- strsplit(selected, '@')[[1]][2]

      id <- which(r_workbench$cohortsSummaryDatabases$databaseId == databaseId & r_workbench$cohortsSummaryDatabases$cohortId == cohortId)
      dbName <- format_str(r_workbench$cohortsSummaryDatabases$databaseName[id])
      cohortName <- format_str(r_workbench$cohortsSummaryDatabases$cohortName[id])

      r_data$databaseId <- databaseId
      r_data$cohortId <- cohortId
      r_data$filename <- paste0(c("cohort", cohortName, "database",  dbName), collapse = "_")

    })

    output$downloadData <- shiny::downloadHandler(
      filename =  function() {r_data$filename},
      content = function(filename) {

        databaseId <- r_data$databaseId
        cohortId <- r_data$cohortId

        sweetAlert_spinner("Preparing cohort for download")

        cohortTableHandler <- r_connectionHandlers$databasesHandlers[[databaseId]]$cohortTableHandler
        connection <- cohortTableHandler$connectionHandler$getConnection()
        cohortDefinitionSet <- cohortTableHandler$cohortDefinitionSet
        cdmDatabaseSchema <- cohortTableHandler$cdmDatabaseSchema
        cohortDatabaseSchema <- cohortTableHandler$cohortDatabaseSchema
        cohortTable <- cohortTableHandler$cohortTableNames$cohortTable
        cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable)

        tryCatch({
          CohortGenerator::createCohortTables(
            connection = connection,
            cohortDatabaseSchema = cohortDatabaseSchema,
            cohortTableNames = cohortTableNames)
        }, error=function(e) {
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Error while creaating a cohort:",
            text = e,
            btn_labels = "OK",
            type = "error"
          )
        }, warning=function(w) {
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Warning while creaating a cohort:",
            text = w,
            btn_labels = "OK",
            type = "warning"
          )
        })

        tryCatch({
          generatedCohorts <- CohortGenerator::generateCohortSet(
            connection = connection,
            cdmDatabaseSchema = cdmDatabaseSchema,
            cohortDatabaseSchema = cohortDatabaseSchema,
            cohortTableNames = cohortTableNames,
            cohortDefinitionSet = cohortDefinitionSet,
            incremental = FALSE)
        }, error=function(e) {
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Error while generating cohort set:",
            text = e,
            btn_labels = "OK",
            type = "error"
          )
        }, warning=function(w) {
          errors$generateCohortSetWarn <- w
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Warning while generating cohort set:",
            text = w,
            btn_labels = "OK",
            type = "warning"
          )
        }, finally = {
          cohortNameIds <- (generatedCohorts |> dplyr::select(cohortId, cohortName))[which(generatedCohorts$cohortId == cohortId), ]
        })

        tryCatch({
          cohortData <- HadesExtras::getCohortDataFromCohortTable(
            connection = connection,
            cdmDatabaseSchema = cdmDatabaseSchema,
            cohortDatabaseSchema = cohortDatabaseSchema,
            cohortTable = cohortTable,
            cohortNameIds = cohortNameIds)
        }, error=function(e) {
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Error while getting cohort data from table:",
            text = e,
            btn_labels = "OK",
            type = "error"
          )
          r_status$success <- FALSE
        }, warning=function(w) {
          errors$getCohortDataFromCohortTableWarn <- w
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Warning while getting cohort data from table:",
            text = w,
            btn_labels = "OK",
            type = "warning"
          )
          r_status$success <- FALSE
        }, finally = {
          write.table(cohortData, filename, row.names=FALSE, sep="\t", quote = F, append = F)
          r_status$success <- TRUE
        })

        remove_sweetAlert_spinner()

      }
    )

    shiny::observe({
      shiny::req(r_status$success)
      shinyWidgets::show_alert(
        title = NULL,
        text = "Download Completed Successfully",
        btn_labels = "OK",
        btn_colors = "#70B6E0",
        width = "550px"
      )
      r_status$success <- NULL
    })


  })


}




