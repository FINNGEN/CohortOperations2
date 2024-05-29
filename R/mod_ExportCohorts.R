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
      cohortDefinitionSet = NULL,
      operationStringError = NULL
    )

    r_toAdd <- shiny::reactiveValues(
      databaseName = NULL,
      cohortDefinitionSet = NULL
    )

    r_data <- shiny::reactiveValues(
      databaseId = NULL,
      databaseName = NULL,
      cohortId = NULL,
      filename = NULL,
      cohortName = NULL,
      success = NULL,
      failedCohorts = NULL
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

      df <- r_workbench$cohortsSummaryDatabases[which(paste0(r_workbench$cohortsSummaryDatabases$databaseId, "@",
                                                             r_workbench$cohortsSummaryDatabases$cohortId) %in% selected), ]

      r_data$databaseId <- df$databaseId
      r_data$cohortId <- df$cohortId
      r_data$cohortName <- df$cohortName
      r_data$databaseName <- df$databaseName
      name <- paste0(paste0(.format_str(df$databaseId), "_", .format_str(df$cohortName)), collapse = "_")
      r_data$filename <- sprintf("%s.csv", name)

    })

    #
    # activate settings if cohors have been selected
    #
    shiny::observe({
      condition <- shiny::isTruthy(input$selectCohorts_pickerInput)
      shinyjs::toggleState("downloadData_downloadButton", condition = condition )
    })

    output$downloadData_downloadButton <- shiny::downloadHandler(
      filename =  function() {r_data$filename},
      content = function(filename) {

        sweetAlert_spinner("Preparing cohort for download")

        result <- rbind()
        for (i in 1:length(r_data$databaseId)){
          databaseId <- r_data$databaseId[i]
          cohortId <- r_data$cohortId[i]
          cohortName <- r_data$cohortName[i]
          cohortTableHandler <- r_connectionHandlers$databasesHandlers[[databaseId]]$cohortTableHandler

          cohortData <- HadesExtras::getCohortDataFromCohortTable(
            connection = cohortTableHandler$connectionHandler$getConnection(),
            cdmDatabaseSchema = cohortTableHandler$cdmDatabaseSchema,
            cohortDatabaseSchema = cohortTableHandler$cohortDatabaseSchema,
            cohortTable = cohortTableHandler$cohortTableNames$cohortTable,
            cohortNameIds = tibble::data_frame(cohortId=cohortId, cohortName=cohortName))

          cohortData <- tibble::add_column(
            cohortData, database_id = rep(databaseId, nrow(cohortData)), .before = 1
          )

          if (is.null(cohortData)){
            r_data$failedCohorts <- c(r_data$failedCohorts,
                                      sprintf("%s (%s)", r_data$cohortName[i],
                                              r_data$databaseName[i]))
          } else {
            result <- rbind(result, cohortData)
          }

        }

        tryCatch({
          write.table(result, filename, row.names=FALSE, sep="\t", quote = F, append = F)
          r_data$success <- TRUE
        }, error=function(e) {
          r_data$success <- FALSE
          ParallelLogger::logError("[Export Cohorts] write table: ", e$message)
        }, warning=function(w) {
          r_data$success <- FALSE
          ParallelLogger::logWarn("[Export Cohorts] write table: ", w$message)
        })

        remove_sweetAlert_spinner()

      }
    )

    shiny::observe({
      shiny::req(!is.null(r_data$success))

      if (!is.null(r_data$failedCohorts)){
        multiple <- length(r_data$failedCohorts) > 1
        message <- sprintf("In addition, the following cohort%s %s not exported: \n%s",
                           if (multiple) "s" else "",
                           if (multiple) "were" else "was",
                           paste(r_data$failedCohorts, collapse = ", "))
      } else {
        message <- ""
      }

      if (r_data$success){
        shinyWidgets::show_alert(
          title = "Download completed successfully",
          text = message,
          btn_labels = "OK",
          btn_colors = "#70B6E0",
          width = "550px"
        )
      } else {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error while downloading a file",
          text = message,
          btn_labels = "OK",
          type = "error"
        )
      }
      r_data$success <- NULL
    })


  })


}


.format_str <- function(x){
  tolower(stringr::str_replace_all(x, "[[:punct:]]", "") %>% stringr::str_replace_all(.,  " ", "_"))
}
