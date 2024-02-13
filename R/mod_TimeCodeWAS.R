


mod_timeCodeWAS_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyjs::useShinyjs(),
    #
    shiny::tags$h4("Database"),
    shinyWidgets::pickerInput(
      inputId = ns("selectDatabases_pickerInput"),
      label = "Select database where to match cohorts:",
      choices = NULL,
      selected = NULL,
      multiple = FALSE),
    htmltools::hr(),
    shiny::tags$h4("Cohorts"),
    shinyWidgets::pickerInput(
      inputId = ns("selectCaseCohort_pickerInput"),
      label = "Select case cohort:",
      choices = NULL,
      selected = NULL,
      multiple = FALSE),
    shinyWidgets::pickerInput(
      inputId = ns("selectControlCohort_pickerInput"),
      label = "Select control cohort:",
      choices = NULL,
      selected = NULL,
      multiple = FALSE),
    htmltools::hr(),
    shiny::tags$h4("Settings"),
    #
    shinyWidgets::pickerInput(
      inputId = ns("selectCovariates"),
      label = "Select database where to match cohorts:",
      choices = list(
        Conditions = list(
          `Conditions` = "useConditionOccurrence",
          `Conditions in Primary Inpatient` = "useConditionOccurrencePrimaryInpatient",
          `Conditions SNOMED Group` = "useConditionEraGroupOverlap"
        ),
        Drugs = list(
          `Drugs` = "useDrugExposure",
          `Drugs ATC Group` = "useDrugEraGroupOverlap"
        ),
        Procedures = list(
          `Procedures` = "useProcedureOccurrence"
        ),
        Others = list(
          `Device Exposure` = "useDeviceExposure",
          `Measurement` = "useMeasurement",
          `Observation` = "useObservation"
        )
      ),
      selected = c("useConditionOccurrence", "useDrugExposure", "useProcedureOccurrence", "useDeviceExposure", "useMeasurement", "useObservation"),
      options = list(`actions-box` = TRUE),
      multiple = TRUE),
    htmltools::hr(),
    mod_formTimeWindows_ui(ns("selectRanges")),
    shiny::tags$br(),
    #
    htmltools::hr(),
    shiny::tags$h4("Summary"),
    shiny::verbatimTextOutput(ns("summary_text")),
    shiny::tags$br(),
    shiny::actionButton(ns("run_actionButton"), "Run Study"),
    #
    htmltools::hr(),
    shiny::tags$h4("Results"),
    shiny::verbatimTextOutput(ns("results_text")),
    shiny::tags$br(),
    shiny::downloadButton(ns("download_actionButton"), "Download to Sandbox"),
    shiny::downloadButton(ns("download_actionButton2"), "Download out of Sandbox"),
    shiny::actionButton(ns("view_actionButton"), "Open Viewer"),
  )
}


mod_timeCodeWAS_server <- function(id, r_connectionHandlers, r_workbench) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns



    #
    # reactive variables
    #
    r_ranges <- mod_formTimeWindows_server("selectRanges")

    r <- shiny::reactiveValues(
      analysisSettings = NULL
    )

    rf_timeCodeWasCounts <- shiny::reactiveVal()

    # A reactive value with the inputs to modalWithLog_server
    .r_l <- shiny::reactiveValues(
      .l = NULL
    )


    #
    # update selectDatabases_pickerInput with database names
    #
    shiny::observe({
      shiny::req(r_connectionHandlers$databasesHandlers)

      databaseIdNamesList <- fct_getDatabaseIdNamesListFromDatabasesHandlers(r_connectionHandlers$databasesHandlers)

      shinyWidgets::updatePickerInput(
        inputId = "selectDatabases_pickerInput",
        choices = databaseIdNamesList,
        selected = databaseIdNamesList[1]
      )
    })

    #
    # update selectCaseCohort_pickerInput with cohort names in selectDatabases_pickerInput database
    #
    shiny::observe({
      shiny::req(r_workbench$cohortsSummaryDatabases)
      shiny::req(input$selectDatabases_pickerInput)

      cohortIdAndNames <- r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandler$getCohortIdAndNames()
      cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, cohortIdAndNames$cohortName))

      shinyWidgets::updatePickerInput(
        inputId = "selectCaseCohort_pickerInput",
        choices = cohortIdAndNamesList,
        selected = character(0)
      )
    })


    #
    # update matchToCohortId_pickerInput with cohort names not in selectCaseCohort_pickerInput
    #
    shiny::observe({
      shiny::req(r_workbench$cohortsSummaryDatabases)
      shiny::req(input$selectDatabases_pickerInput)
      shiny::req(input$selectCaseCohort_pickerInput)

      cohortTableHandler <- r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandler

      if(input$selectCaseCohort_pickerInput != "NA"){
        cohortIdAndNames <- cohortTableHandler$getCohortIdAndNames() |>
          dplyr::filter(!(cohortId %in% input$selectCaseCohort_pickerInput))
        cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, cohortIdAndNames$cohortName))
      }else{
        cohortIdAndNamesList <- list()
      }


      shinyWidgets::updatePickerInput(
        inputId = "selectControlCohort_pickerInput",
        choices = cohortIdAndNamesList,
        selected = character(0)
      )
    })

    #
    # activate settings if cohors have been selected
    #
    shiny::observe({
      condition <- !is.null(input$selectControlCohort_pickerInput) & input$selectControlCohort_pickerInput!="NA"
      shinyjs::toggleState("selectCovariates", condition = condition )
      shinyjs::toggleState("selectRanges-addBtn", condition = condition )
      shinyjs::toggleState("run_actionButton", condition = condition )
    })

    #
    # create settings
    #
    shiny::observe({
      shiny::req(input$selectDatabases_pickerInput)
      shiny::req(input$selectCaseCohort_pickerInput)
      shiny::req(input$selectCaseCohort_pickerInput!="NA")
      shiny::req(input$selectControlCohort_pickerInput)
      shiny::req(input$selectControlCohort_pickerInput!="NA")
      shiny::req(input$selectCovariates)
      shiny::req(r_ranges()$temporalStartDays)
      shiny::req(r_ranges()$temporalEndDays)

      analysisSettings <- list(
        analysisType = "timeCodeWAS",
        cohortIdCases = input$selectCaseCohort_pickerInput,
        cohortIdControls = input$selectControlCohort_pickerInput,
        temporalCovariateSettings = list(
          temporalStartDays = r_ranges()$temporalStartDays,
          temporalEndDays =   r_ranges()$temporalEndDays
        )
      )

      for(covaraiteSetting in input$selectCovariates){
        analysisSettings$temporalCovariateSettings[[covaraiteSetting]] <- TRUE
      }

      r$analysisSettings <- analysisSettings

    })

    #
    # Render temporal name
    #
    output$newCohortName_text <- shiny::renderText({
      if(!shiny::isTruthy(r$analysisSettings)){
        "----"
      }else{
        yaml::as.yaml(r$analysisSettings)
      }
    })

    #
    # click to run
    #
    shiny::observeEvent(input$run_actionButton, {
      shiny::req(r$analysisSettings)
      # copy analysisSettings to .r_l$.l
      cohortTableHandler <- r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandler

      l <- r$analysisSettings

      .r_l$.l <- list(
        cohortTableHandler = cohortTableHandler,
        analysisSettings = l,
        sqlRenderTempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
      )
    })


    # Take parameters, run function in a future, open modal with log, close modal when ready, return value
    rf_results <- modalWithLog_server(
      id = "sss",
      .f = function(
          cohortTableHandler,
          analysisSettings,
          sqlRenderTempEmulationSchema
        ){
        # needs to be set in the future
        options(sqlRenderTempEmulationSchema=sqlRenderTempEmulationSchema)
        #
        ParallelLogger::logInfo("Create tmp folder")
        tmpdirTime <- file.path(tempdir(), format(Sys.time(), "%Y%m%d_%H%M%S"))
        dir.create(tmpdirTime)
        ParallelLogger::logInfo(tmpdirTime)
        #
        ParallelLogger::logInfo("Start timeCodeWasCounts")
        temporalCovariateSettings <- do.call(FeatureExtraction::createTemporalCovariateSettings, analysisSettings$temporalCovariateSettings)
        #browser()
        HadesExtras::executeTimeCodeWAS(
          exportFolder = tmpdirTime,
          cohortTableHandler = cohortTableHandler,
          cohortIdCases = as.integer(analysisSettings$cohortIdCases),
          cohortIdControls = as.integer(analysisSettings$cohortIdControls),
          covariateSettings = temporalCovariateSettings,
          minCellCount = analysisSettings$minCellCount
        )

        ParallelLogger::logInfo("Merging results")
        yaml::write_yaml(analysisSettings, file.path(tmpdirTime, "analysisSettings.yaml"))

        # zip all files
        analysisResultsZipPath <- file.path(tmpdirTime, "analysisResults.zip")
        zip::zipr(zipfile = analysisResultsZipPath, files = list.files(tmpdirTime, full.names = TRUE, recursive = TRUE))

        ParallelLogger::logInfo("End timeCodeWasCounts")

        return(analysisResultsZipPath)
      },
      .r_l = .r_l,
      logger = shiny::getShinyOption("logger"))


    #
    # display results
    #
    output$summary_text <- shiny::renderText({
      rf_results()$result
      shiny::req(rf_results)

      if(rf_results()$success){
        shiny::removeModal()
      }

      rf_results()$result
    })

    #
    # activate settings if cohorts have been selected
    #
    shiny::observe({
      tryCatch({
        condition <- shiny::isTruthy(rf_results()$success)
      }, error = function(e){
        condition <<- FALSE
      })

      shinyjs::toggleState("download_actionButton", condition = condition )
      shinyjs::toggleState("download_actionButton2", condition = FALSE )
      shinyjs::toggleState("view_actionButton", condition = condition )
    })


    output$download_actionButton <- shiny::downloadHandler(
      filename = function(){"analysisName_timeCodeWAS.zip"},
      content = function(fname){

        if(rf_results()$success){
          file.copy(rf_results()$result, fname)
        }

        return(fname)
      }
    )


    shiny::observeEvent(input$view_actionButton, {
      shiny::req(rf_results())
      shiny::req(rf_results()$success)
      # open tab to url
      url <- paste0("http://127.0.0.1:8111/?pathToResultsZip=", rf_results()$result)
      browseURL(url)

    })



  })
}





















