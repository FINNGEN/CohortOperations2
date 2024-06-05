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
mod_runGWAS_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyjs::useShinyjs(),
    shiny::tags$h4("Cohort"),
    shiny::tags$h5("Select cohort:"),
    shinyWidgets::pickerInput(
      inputId = ns("selectCasesCohort_pickerInput"),
      width = "600px",
      label = NULL,
      choices = NULL,
      selected = NULL,
      options = list(
        `actions-box` = TRUE),
      multiple = FALSE),
    htmltools::hr(),
    shinyWidgets::pickerInput(
      inputId = ns("selectControlsCohort_pickerInput"),
      width = "600px",
      label = NULL,
      choices = NULL,
      selected = NULL,
      options = list(
        `actions-box` = TRUE),
      multiple = FALSE),
    htmltools::hr(),
    shiny::actionButton(ns("run_actionButton"), "Run GWAS Analysis"),
    htmltools::hr()
  )
}


mod_runGWAS_server <- function(id, r_connectionHandlers, r_workbench) {

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      cohortDefinitionSet = NULL,
      operationStringError = NULL
    )

    r_data <- shiny::reactiveValues(
      casesDatabaseId = NULL,
      controlsDatabaseId = NULL,
      casesDatabaseName = NULL,
      controlsDatabaseName = NULL,
      casesCohortId = NULL,
      controlsCohortId = NULL,
      casesCohortName = NULL,
      controlsCohortName = NULL,
      success = NULL,
      phenotypeName = NULL
    )

    #
    # render cohort pickers with database/cohort names
    #
    shiny::observeEvent(r_workbench$cohortsSummaryDatabases, {

      cohortIdAndNamesList <- list()
      for(databaseId in unique(r_workbench$cohortsSummaryDatabases$databaseId)){
        cohortIdAndNames <- r_workbench$cohortsSummaryDatabases |>
          dplyr::filter(databaseId == !!databaseId) |>
          dplyr::select(cohortId, shortName)
        cohortIdAndNamesList[databaseId] <- list(
          as.list(setNames(paste0(databaseId, "@", cohortIdAndNames$cohortId), cohortIdAndNames$shortName))
        )
      }
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "selectCasesCohort_pickerInput",
        choices = cohortIdAndNamesList,
        selected = NULL
      )
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "selectControlsCohort_pickerInput",
        choices = cohortIdAndNamesList,
        selected = NULL
      )
    })


    shiny::observeEvent(input$selectCasesCohort_pickerInput, {
      shiny::req(r_workbench$cohortsSummaryDatabases)
      shiny::req(input$selectCasesCohort_pickerInput)

      selected <- input$selectCasesCohort_pickerInput

      ids <- which(paste0(r_workbench$cohortsSummaryDatabases$databaseId, "@",
                          r_workbench$cohortsSummaryDatabases$cohortId) %in% selected)

      cohort <- r_workbench$cohortsSummaryDatabases[ids, ]

      r_data$casesDatabaseId <- cohort$databaseId
      r_data$casesDatabaseName <- cohort$databaseName
      r_data$casesCohortId <- cohort$cohortId
      r_data$casesCohortName <- cohort$cohortName

    })

    shiny::observeEvent(input$selectControlsCohort_pickerInput, {
      shiny::req(r_workbench$cohortsSummaryDatabases)
      shiny::req(input$selectControlsCohort_pickerInput)

      selected <- input$selectControlsCohort_pickerInput

      ids <- which(paste0(r_workbench$cohortsSummaryDatabases$databaseId, "@",
                          r_workbench$cohortsSummaryDatabases$cohortId) %in% selected)

      cohort <- r_workbench$cohortsSummaryDatabases[ids, ]

      r_data$controlsDatabaseId <- cohort$databaseId
      r_data$controlsDatabaseName <- cohort$databaseName
      r_data$controlsCohortId <- cohort$cohortId
      r_data$controlsCohortName <- cohort$cohortName

    })

    #
    # activate settings if cohorts have been selected
    #
    shiny::observe({
      condition <- shiny::isTruthy((!is.null(input$selectCasesCohort_pickerInput) &
                                      !is.null(input$selectControlsCohort_pickerInput)) &
                                     (input$selectCasesCohort_pickerInput != input$selectControlsCohort_pickerInput))
      shinyjs::toggleState("run_actionButton", condition = condition )
    })

    #
    # click to run
    #
    shiny::observeEvent(input$run_actionButton, {
      shiny::req(!is.null(r_data$casesCohortName))
      shiny::req(!is.null(r_data$controlsCohortName))

      r_data$phenotypeName <- paste0(.format_str(r_data$casesCohortName), .format_str(r_data$controlsCohortName))

      sweetAlert_spinner("Preparing cohorts for submission to GWAS analysis")

      casesCohortTableHandler <- r_connectionHandlers$databasesHandlers[[r_data$casesDatabaseId]]$cohortTableHandler

      casesCohortData <- HadesExtras::getCohortDataFromCohortTable(
        connection = casesCohortTableHandler$connectionHandler$getConnection(),
        cdmDatabaseSchema = casesCohortTableHandler$cdmDatabaseSchema,
        cohortDatabaseSchema = casesCohortTableHandler$cohortDatabaseSchema,
        cohortTable = casesCohortTableHandler$cohortTableNames$cohortTable,
        cohortNameIds = tibble::data_frame(cohortId=r_data$casesCohortId, cohortName=r_data$casesCohortName))

      controlsCohortTableHandler <- r_connectionHandlers$databasesHandlers[[r_data$controlsDatabaseId]]$cohortTableHandler

      controlsCohortData <- HadesExtras::getCohortDataFromCohortTable(
        connection = controlsCohortTableHandler$connectionHandler$getConnection(),
        cdmDatabaseSchema = controlsCohortTableHandler$cdmDatabaseSchema,
        cohortDatabaseSchema = controlsCohortTableHandler$cohortDatabaseSchema,
        cohortTable = controlsCohortTableHandler$cohortTableNames$cohortTable,
        cohortNameIds = tibble::data_frame(cohortId=r_data$controlsCohortId, cohortName=r_data$controlsCohortName))

      cases_cohort <- list(
        name = r_data$casesCohortName,
        validated_ids = casesCohortData$person_source_value
      )

      controls_cohort <- list(
        name = r_data$controlsCohortName,
        validated_ids = controlsCohortData$person_source_value
      )

      cohorts_settings <- list(
        cases_cohort = cases_cohort,
        controls_cohort = controls_cohort
      )

      tryCatch({
        FinnGenUtilsR::runGWASAnalysis(
          r_connectionHandlers$connection_sandboxAPI,
          cohorts_settings,
          phenotype_name,
          title = phenotype_name,
          notification_email = r_connectionHandlers$connection_sandboxAPI$notification_email
        )
        r_data$success <- TRUE
      }, error=function(e) {
        r_data$success <- FALSE
        ParallelLogger::logError("[Run GWAS analysis]: ", e$message)
      }, warning=function(w) {
        r_data$success <- FALSE
        ParallelLogger::logWarn("[Run GWAS analysis]:", w$message)
      })

      remove_sweetAlert_spinner()

    })

    shiny::observe({
      shiny::req(!is.null(r_data$success))

      if (r_data$success){
        shinyWidgets::show_alert(
          title = "GWAS run submitted successfully",
          text = paste0("Phenotype name: ", r_data$phenotypeName),
          btn_labels = "OK",
          btn_colors = "#70B6E0",
          width = "550px"
        )
      } else {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error while submitting GWAS run",
          btn_labels = "OK",
          type = "error"
        )
      }
      r_data$success <- NULL
    })


  })


}


.format_str <- function(x){
  toupper(stringr::str_replace_all(x, "[[:punct:]]|[^[:alnum:]]|[:blank:]", ""))
}

