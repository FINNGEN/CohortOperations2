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
#' @importFrom shinyFeedback useShinyFeedback
mod_runGWAS_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyFeedback::useShinyFeedback(),
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
    shiny::textInput(ns("pheno"), label = "Phenotype Name:"),
    shiny::textInput(ns("description"), label = "Description:"),
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
    # setup warning on input for the phenotype name
    #
    shiny::observeEvent(input$pheno, {
      shinyFeedback::feedbackWarning(
        inputId = "pheno",
        stringr::str_detect(input$pheno, "[^[:alnum:]]|[:lower:]"),
        text = "Name must use only upper case characters or numbers"
      )
    })

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

    #
    # render cases cohort picker
    #
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

      defaultPhenotypeName <- paste0(.format_str(r_data$casesCohortName), .format_str(r_data$controlsCohortName))
      shiny::updateTextInput(session, "pheno", value = defaultPhenotypeName )

    })

    #
    # render controls cohort picker
    #
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

      defaultPhenotypeName <- paste0(.format_str(r_data$casesCohortName), .format_str(r_data$controlsCohortName))
      shiny::updateTextInput(session, "pheno", value = defaultPhenotypeName )

      defaultDescription <- paste0("Cases-cohort: ",
                                   r_data$casesCohortName,
                                   "(db: ", r_data$casesDatabaseName, "); ",
                                   "Controls-cohort: ", r_data$controlsCohortName,
                                   "(db: ", r_data$controlsDatabaseName, ")")
      shiny::updateTextInput(session, "description", value = defaultDescription )

    })

    #
    # activate settings if cohorts have been selected
    #
    shiny::observe({
      condition <- shiny::isTruthy(
        (!is.null(input$selectCasesCohort_pickerInput) & !is.null(input$selectControlsCohort_pickerInput)) &
        (input$selectCasesCohort_pickerInput != input$selectControlsCohort_pickerInput) &
        (stringr::str_detect(input$pheno, "[^[:alnum:]]|[:lower:]", negate = T))
      )
      shinyjs::toggleState("run_actionButton", condition = condition )
    })

    #
    # click to run
    #
    shiny::observeEvent(input$run_actionButton, {

      r_data$phenotypeName <- input$pheno

      sweetAlert_spinner("Preparing cohorts for submission to GWAS analysis")

      casesCohortTableHandler <- r_connectionHandlers$databasesHandlers[[r_data$casesDatabaseId]]$cohortTableHandler

      casesCohortData <- HadesExtras::getCohortDataFromCohortTable(
        connection = casesCohortTableHandler$connectionHandler$getConnection(),
        cdmDatabaseSchema = casesCohortTableHandler$cdmDatabaseSchema,
        cohortDatabaseSchema = casesCohortTableHandler$cohortDatabaseSchema,
        cohortTable = casesCohortTableHandler$cohortTableNames$cohortTable,
        cohortNameIds = tibble::tibble(cohortId=r_data$casesCohortId, cohortName=r_data$casesCohortName))

      controlsCohortTableHandler <- r_connectionHandlers$databasesHandlers[[r_data$controlsDatabaseId]]$cohortTableHandler

      controlsCohortData <- HadesExtras::getCohortDataFromCohortTable(
        connection = controlsCohortTableHandler$connectionHandler$getConnection(),
        cdmDatabaseSchema = controlsCohortTableHandler$cdmDatabaseSchema,
        cohortDatabaseSchema = controlsCohortTableHandler$cohortDatabaseSchema,
        cohortTable = controlsCohortTableHandler$cohortTableNames$cohortTable,
        cohortNameIds = tibble::tibble(cohortId=r_data$controlsCohortId, cohortName=r_data$controlsCohortName))

      ParallelLogger::logInfo("[Run GWAS analysis]: Submitting GWAS analysis with phenotype name ", input$pheno)

      res <- FinnGenUtilsR::runGWASAnalysis(
        connection_sandboxAPI = r_connectionHandlers$connection_sandboxAPI,
        cases_finngenids = casesCohortData$person_source_value,
        controls_finngenids = controlsCohortData$person_source_value,
        phenotype_name = input$pheno,
        title = input$pheno,
        description = input$description,
        notification_email = r_connectionHandlers$connection_sandboxAPI$notification_email
      )

      r_data$success <- res$status

      if (res$status){
        ParallelLogger::logInfo("[Run GWAS analysis]:  successfully submitted")
      } else {
        ParallelLogger::logError("[Run GWAS analysis]:  ", res$content)
      }

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

