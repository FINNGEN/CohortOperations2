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
    shiny::tags$h4("Database"),
    shiny::tags$h5("Select database where to run GWAS analysis:"),
    shinyWidgets::pickerInput(
      inputId = ns("selectDatabases_pickerInput_gwas"),
      choices = NULL,
      selected = NULL,
      multiple = FALSE),
    htmltools::hr(),
    shiny::tags$h4("Cohorts"),
    shiny::tags$h5("Select case cohort:"),
    shinyWidgets::pickerInput(
      inputId = ns("selectCasesCohort_pickerInput"),
      label = NULL,
      choices = NULL,
      selected = NULL,
      options = list(
        `actions-box` = TRUE),
      multiple = FALSE),
    shiny::tags$h5("Select control cohort:"),
    shinyWidgets::pickerInput(
      inputId = ns("selectControlsCohort_pickerInput"),
      label = NULL,
      choices = NULL,
      selected = NULL,
      options = list(
        `actions-box` = TRUE),
      multiple = FALSE),
    htmltools::hr(),
    shiny::tags$h4("Settings"),
    shiny::tags$h5("Select analysis type:"),
    shinyWidgets::pickerInput(
      inputId = ns("selectAnalysisType_pickerInput_gwas"),
      choices = c("additive", "recessive", "dominant"),
      selected = "additive",
      multiple = FALSE),
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
      databaseId = NULL,
      databaseName = NULL,
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
    # update selectDatabases_pickerInput_gwas with database names
    #
    shiny::observe({
      shiny::req(r_connectionHandlers$databasesHandlers)

      databaseIdNamesList <- fct_getDatabaseIdNamesListFromDatabasesHandlers(r_connectionHandlers$databasesHandlers)

      shinyWidgets::updatePickerInput(
        inputId = "selectDatabases_pickerInput_gwas",
        choices = databaseIdNamesList,
        selected = databaseIdNamesList[1]
      )

      r_data$databaseId <- names(databaseIdNamesList[1])
      r_data$databaseName <- databaseIdNamesList[[1]]

    })

    #
    # render database selection from picker
    #
    shiny::observeEvent(input$selectDatabases_pickerInput_gwas, {
      shiny::req(input$selectDatabases_pickerInput_gwas)
      databaseIdNamesList <- fct_getDatabaseIdNamesListFromDatabasesHandlers(r_connectionHandlers$databasesHandlers)
      selected <- databaseIdNamesList[which(databaseIdNamesList == input$selectDatabases_pickerInput_gwas)]
      r_data$databaseName <- names(selected)
      r_data$databaseId <- as.character(selected)
    })

    #
    # render cohort pickers with database/cohort names
    #
    shiny::observe({
      shiny::req(r_workbench$cohortsSummaryDatabases)
      shiny::req(input$selectDatabases_pickerInput_gwas)

      cohorts <- r_workbench$cohortsSummaryDatabases[
        which(r_workbench$cohortsSummaryDatabases$databaseId == input$selectDatabases_pickerInput_gwas),
      ]
      cohortsList <- cohorts$cohortName

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "selectCasesCohort_pickerInput",
        choices = cohortsList,
        selected = cohortsList[1]
      )

    })

    #
    # render cases cohort picker
    #
    shiny::observe({
      shiny::req(r_workbench$cohortsSummaryDatabases)
      shiny::req(input$selectDatabases_pickerInput_gwas)
      shiny::req(!is.null(input$selectCasesCohort_pickerInput))

      cohorts <- r_workbench$cohortsSummaryDatabases[
        which(r_workbench$cohortsSummaryDatabases$databaseId == input$selectDatabases_pickerInput_gwas),
      ]

      cohortsList <- cohorts$cohortName[cohorts$cohortName != input$selectCasesCohort_pickerInput]

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "selectControlsCohort_pickerInput",
        choices = cohortsList,
        selected = cohortsList[1]
      )

    })

    #
    # update phenotype name and description with default values
    #
    shiny::observe({
      shiny::req(!is.null(input$selectCasesCohort_pickerInput))
      shiny::req(!is.null(input$selectControlsCohort_pickerInput))

      defaultPhenotypeName <- paste0(
        .format_str(input$selectCasesCohort_pickerInput),
        .format_str(input$selectControlsCohort_pickerInput)
      )

      defaultDescription <- paste0(
        "Cases-cohort: ", input$selectCasesCohort_pickerInput,
        "; Controls-cohort: ", input$selectControlsCohort_pickerInput,
        " (db: ", r_data$databaseName, ")"
      )

      shiny::updateTextInput(session, "pheno", value = defaultPhenotypeName )
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
    # click to run GWAS
    #
    shiny::observeEvent(input$run_actionButton, {

      sweetAlert_spinner("Preparing cohorts for submission to GWAS analysis")

      cohortTableHandler <- r_connectionHandlers$databasesHandlers[[r_data$databaseId]]$cohortTableHandler
      cohorts <- r_workbench$cohortsSummaryDatabases[ r_workbench$cohortsSummaryDatabases$databaseId == r_data$databaseId, ]
      casesCohort <- cohorts[cohorts$cohortName == input$selectCasesCohort_pickerInput, ]
      controlsCohort <- cohorts[cohorts$cohortName == input$selectControlsCohort_pickerInput, ]

      cohortData <- HadesExtras::getCohortDataFromCohortTable(
        connection = cohortTableHandler$connectionHandler$getConnection(),
        cdmDatabaseSchema = cohortTableHandler$cdmDatabaseSchema,
        cohortDatabaseSchema = cohortTableHandler$cohortDatabaseSchema,
        cohortTable = cohortTableHandler$cohortTableNames$cohortTable,
        cohortNameIds = tibble::tibble(
          cohortId = c(casesCohort$cohortId, controlsCohort$cohortId),
          cohortName = c(casesCohort$cohortName, controlsCohort$cohortName))
      )

      cases_finngenids <- cohortData$person_source_value[
        which(cohortData$cohort_name == casesCohort$cohortName)
      ]

      controls_finngenids <- cohortData$person_source_value[
        which(cohortData$cohort_name == controlsCohort$cohortName)
      ]

      releaseVersion <- gsub("[A-Za-z]", "", r_data$databaseId)
      release <- if(as.numeric(releaseVersion) < 7 | as.numeric(releaseVersion) > 12) NULL else paste0("Regenie", releaseVersion)
      ParallelLogger::logInfo("[Run GWAS analysis]: using Regenie version ", release)

      res <- FinnGenUtilsR::runGWASAnalysis(
        connection_sandboxAPI = r_connectionHandlers$connection_sandboxAPI,
        cases_finngenids = cases_finngenids,
        controls_finngenids = controls_finngenids,
        phenotype_name = input$pheno,
        title = input$pheno,
        description = input$description,
        notification_email = r_connectionHandlers$connection_sandboxAPI$notification_email,
        analysis_type = input$selectAnalysisType_pickerInput_gwas,
        release = release
      )

      r_data$success <- res$status
      r_data$phenotypeName <- input$pheno

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

