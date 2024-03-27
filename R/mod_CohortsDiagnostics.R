


mod_cohortDiagnostics_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyjs::useShinyjs(),
    #
    shiny::tags$h4("Cohorts"),
    shinyWidgets::pickerInput(
      inputId = ns("selectCohort_pickerInput"),
      label = "Select one or more cohorts:",
      choices = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    #
    htmltools::hr(),
    shiny::tags$h4("Settings"),
    shiny::numericInput(
      inputId = ns("minCellCount_numericInput"),
      label = "Min Cell Count",
      width = "100px",
      value = 1,
      min = 1,
      max = 1000
    ),
    shiny::checkboxInput(
      inputId = ns("runInclusionStatistics_switch"),
      label = "Run Inclusion Statistics (for Atlas cohorts, calculates attrition plot)",
      value = FALSE
    ),
    shiny::checkboxInput(
      inputId = ns("runIncludedSourceConcepts_switch"),
      label = "Run Included Source Concepts",
      value = FALSE
    ),
    shiny::checkboxInput(
      inputId = ns("runOrphanConcepts_switch"),
      label = "Run Orphan Concepts (for Atlas cohorts, suggest concepts to add to cohort definition)",
      value = FALSE
    ),
    shiny::checkboxInput(
      inputId = ns("runVisitContext_switch"),
      label = "Run Visit Context (Visit types around the cohort start)",
      value = FALSE
    ),
    shiny::checkboxInput(
      inputId = ns("runIncidenceRate_switch"),
      label = "Run Incidence Rate",
      value = TRUE
    ),
    # shiny::checkboxInput(
    #   inputId = ns("runCohortRelationship_switch"),
    #   label = "Run Cohort Relationship",
    #   value = FALSE
    # ),
    shiny::checkboxInput(
      inputId = ns("runTemporalCohortCharacterization_switch"),
      label = "Run Temporal Cohort Characterization (Find covarates in time windows)",
      value = FALSE
    ),
    htmltools::hr(),
    #
    shinyWidgets::pickerInput(
      inputId = ns("selectCovariates"),
      label = "Select covariates:",
      choices = list(
        Demographic = list(
          `Gender` = "useDemographicsGender",
          `Age at cohort start` = "useDemographicsAge",
          `Age decile at cohort start` = "useDemographicsAgeGroup",
          `Year at cohort start` = "useDemographicsIndexYear",
          `Days with observation before cohort start` = "useDemographicsPriorObservationTime",
          `Days with observation after cohort start` = "useDemographicsPostObservationTime"
        ),
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
      selected = c(
        "useDemographicsGender", "useDemographicsAge", "useDemographicsAgeGroup", "useDemographicsIndexYear",
        "useDemographicsPriorObservationTime", "useDemographicsPostObservationTime",
        "useConditionOccurrence", "useDrugExposure", "useProcedureOccurrence", "useDeviceExposure", "useMeasurement", "useObservation"
        ),
      options = list(`actions-box` = TRUE),
      multiple = TRUE),
    #
    shinyWidgets::pickerInput(
      inputId = ns("selectSourceCovariates"),
      label = "Select source covariates:",
      choices = list(
        Conditions = list(
          `Conditions` = "useConditionOccurrenceSourceConcept"
        ),
        Drugs = list(
          `Drugs` = "useDrugExposureSourceConcept"
        ),
        Procedures = list(
          `Procedures` = "useProcedureOccurrenceSourceConcept"
        ),
        Others = list(
            `Device Exposure` = "useDeviceExposureSourceConcept",
            `Measurement` = "useMeasurementSourceConcept",
            `Observation` = "useObservationSourceConcept"
        )
      ),
      selected = c("useConditionOccurrenceSourceConcept", "useDrugExposureSourceConcept", "useProcedureOccurrenceSourceConcept", "useDeviceExposureSourceConcept", "useMeasurementSourceConcept", "useObservationSourceConcept"),
      options = list(`actions-box` = TRUE),
      multiple = TRUE),
    #
    htmltools::hr(),
    shiny::tags$h4("Time windows"),
    mod_temporalRanges_ui(ns("time_windows")),
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


mod_cohortDiagnostics_server <- function(id, r_connectionHandlers, r_workbench) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #
    startRanges = list(c(-50000,50000), c(0,0), c(-50000,-1), c(1,50000))
    r_ranges <- mod_temporalRanges_server("time_windows", startRanges = startRanges)

    r <- shiny::reactiveValues(
      analysisSettings = NULL
    )

    rf_sqliteDbPath <- shiny::reactiveVal()

    # A reactive value with the inputs to modalWithLog_server
    .r_l <- shiny::reactiveValues(
      .l = NULL
    )


    #
    # render selectDatabases_pickerInput with database names
    #
    shiny::observeEvent(r_workbench$cohortsSummaryDatabases, {

      cohortIdAndNamesList <- list()
      for(databaseId in unique(r_workbench$cohortsSummaryDatabases$databaseId)){
        cohortIdAndNames <- r_workbench$cohortsSummaryDatabases |> dplyr::filter(databaseId == !!databaseId) |> dplyr::select(cohortId, shortName)
        cohortIdAndNamesList[databaseId] <- list(as.list(setNames(paste0(databaseId, "@", cohortIdAndNames$cohortId), cohortIdAndNames$shortName)))
      }

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "selectCohort_pickerInput",
        choices = cohortIdAndNamesList,
        selected = NULL
      )
    })


    #
    # activate settings if cohort have been selected
    #
    shiny::observe({
      condition <- !is.null(input$selectCohort_pickerInput)
      shinyjs::toggleState("run_actionButton", condition = condition )
      shinyjs::toggleState("minCellCount_numericInput", condition = condition )
      shinyjs::toggleState("runInclusionStatistics_switch", condition = condition )
      shinyjs::toggleState("runIncludedSourceConcepts_switch", condition = condition )
      shinyjs::toggleState("runOrphanConcepts_switch", condition = condition )
      shinyjs::toggleState("runVisitContext_switch", condition = condition )
      shinyjs::toggleState("runIncidenceRate_switch", condition = condition )
      #shinyjs::toggleState("runCohortRelationship_switch", condition = condition )
      shinyjs::toggleState("runTemporalCohortCharacterization_switch", condition = condition )
    })

    #
    # activate covariates if runTemporalCohortCharacterization
    #
    shiny::observe({
      condition <- input$runTemporalCohortCharacterization_switch
      shinyjs::toggleState("selectCovariates", condition = condition )
      shinyjs::toggleState("selectSourceCovariates", condition = condition )
    })

    #
    # create settings
    #
    shiny::observe({
      shiny::req(input$selectCohort_pickerInput)
      shiny::req(input$selectCohort_pickerInput!="NA")
      shiny::req(input$minCellCount_numericInput)
      shiny::req(!is.null(input$runInclusionStatistics_switch))
      shiny::req(!is.null(input$runIncludedSourceConcepts_switch))
      shiny::req(!is.null(input$runOrphanConcepts_switch))
      shiny::req(!is.null(input$runVisitContext_switch))
      shiny::req(!is.null(input$runIncidenceRate_switch))
      #shiny::req(!is.null(input$runCohortRelationship_switch))
      shiny::req(!is.null(input$runTemporalCohortCharacterization_switch))

      # convert vector of strings databaseId-cohortId to tibble databaseId and cohortIds
      databaseIdsCohortIdsTibble<- data.frame(
        databaseId = gsub(pattern = "@.*", replacement = "", x = input$selectCohort_pickerInput),
        cohortId = gsub(pattern = ".*@", replacement = "", x = input$selectCohort_pickerInput)
      )

      databaseIdsCohorsIdsList <- list()
      for(databaseId in unique(databaseIdsCohortIdsTibble$databaseId)){
        databaseIdsCohorsIdsList[[databaseId]] <- databaseIdsCohortIdsTibble |> dplyr::filter(databaseId == !!databaseId) |> dplyr::pull(cohortId)
      }

      covariateSettings  <- NULL
      # standard covariate settings
      if(length(input$selectCovariates) != 0){
        covariateSettings <- FeatureExtraction::createTemporalCovariateSettings(
          useDemographicsGender = 'useDemographicsGender' %in% input$selectCovariates,
          useDemographicsAge = 'useDemographicsAge' %in% input$selectCovariates,
          useDemographicsAgeGroup = 'useDemographicsAgeGroup' %in% input$selectCovariates,
          useDemographicsIndexYear = 'useDemographicsIndexYear' %in% input$selectCovariates,
          useDemographicsPriorObservationTime = 'useDemographicsPriorObservationTime' %in% input$selectCovariates,
          useDemographicsPostObservationTime = 'useDemographicsPostObservationTime' %in% input$selectCovariates,
          useConditionOccurrence = 'useConditionOccurrence' %in% input$selectCovariates,
          useConditionOccurrencePrimaryInpatient = 'useConditionOccurrencePrimaryInpatient' %in% input$selectCovariates,
          useConditionEraGroupOverlap = 'useConditionEraGroupOverlap' %in% input$selectCovariates,
          useDrugExposure = 'useDrugExposure' %in% input$selectCovariates,
          useDrugEraGroupOverlap = 'useDrugEraGroupOverlap' %in% input$selectCovariates,
          useProcedureOccurrence = 'useProcedureOccurrence' %in% input$selectCovariates,
          useDeviceExposure = 'useDeviceExposure' %in% input$selectCovariates,
          useMeasurement = 'useMeasurement' %in% input$selectCovariates,
          useObservation = 'useObservation' %in% input$selectCovariates,
          temporalStartDays = r_ranges()$temporalStartDays,
          temporalEndDays =   r_ranges()$temporalEndDays
        )
      }

      # source covariate settings
        sourceCovariateSettings <- NULL
      if(length(input$selectSourceCovariates) != 0){
        sourceCovariateSettings <- HadesExtras::FeatureExtraction_createTemporalSourceCovariateSettings(
          useConditionOccurrenceSourceConcept = 'useConditionOccurrenceSourceConcept' %in% input$selectSourceCovariates,
          useDrugExposureSourceConcept = 'useDrugExposureSourceConcept' %in% input$selectSourceCovariates,
          useProcedureOccurrenceSourceConcept = 'useProcedureOccurrenceSourceConcept' %in% input$selectSourceCovariates,
          useDeviceExposureSourceConcept = 'useDeviceExposureSourceConcept' %in% input$selectSourceCovariates,
          useMeasurementSourceConcept = 'useMeasurementSourceConcept' %in% input$selectSourceCovariates,
          useObservationSourceConcept = 'useObservationSourceConcept' %in% input$selectSourceCovariates,
          temporalStartDays = r_ranges()$temporalStartDays,
          temporalEndDays =   r_ranges()$temporalEndDays
        )
      }

      allCovariateSettings <- list(
        if(!is.null(covariateSettings)) covariateSettings,
        if(!is.null(sourceCovariateSettings)) sourceCovariateSettings
      )


      analysisSettings <- list(
        analysisType = "cohortDiagnostics",
        databaseIdsCohorsIdsList = databaseIdsCohorsIdsList,
        minCellCount = input$minCellCount_numericInput,
        runInclusionStatistics = input$runInclusionStatistics_switch,
        runIncludedSourceConcepts = input$runIncludedSourceConcepts_switch,
        runOrphanConcepts = input$runOrphanConcepts_switch,
        runVisitContext = input$runVisitContext_switch,
        runIncidenceRate = input$runIncidenceRate_switch,
        runCohortRelationship =  FALSE,
        runTemporalCohortCharacterization = input$runTemporalCohortCharacterization_switch,
        runBreakdownIndexEvents = FALSE, # always FALSE, at the moment
        runTimeSeries = FALSE, # always FALSE, at the moment
        temporalCovariateSettings = allCovariateSettings
      )

      r$analysisSettings <- analysisSettings
    })

    #
    # Render temporal name
    #
    output$summary_text <- shiny::renderText({
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
      databasesHandlers <- r_connectionHandlers$databasesHandlers


      l <- r$analysisSettings

      .r_l$.l <- list(
        databasesHandlers = databasesHandlers,
        analysisSettings = l,
        sqlRenderTempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
      )
    })


    # Take parameters, run function in a future, open modal with log, close modal when ready, return value
    rf_results <- modalWithLog_server(
      id = "sss",
      .f = function(
    databasesHandlers,
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
        ParallelLogger::logInfo("Start cohortDiagnostics")
        pathToResultFolders <- c()
        for(databaseId in names(analysisSettings$databaseIdsCohorsIdsList)){
          tmpdirTimeDatabase <- file.path(tmpdirTime, databaseId)
          dir.create(tmpdirTimeDatabase)

          ParallelLogger::logInfo("databaseId = ", databaseId)
          cohortTableHandler <-databasesHandlers[[databaseId]]$cohortTableHandler
          exportFolder  <-  tmpdirTimeDatabase
          pathToResultFolders <- c(pathToResultFolders, exportFolder)

          CohortDiagnostics::executeDiagnostics(
            cohortDefinitionSet = cohortTableHandler$cohortDefinitionSet,
            exportFolder = exportFolder,
            databaseId = cohortTableHandler$databaseName,
            cohortDatabaseSchema = cohortTableHandler$cohortDatabaseSchema,
            databaseName = cohortTableHandler$databaseName,
            databaseDescription = cohortTableHandler$databaseName,
            connection = cohortTableHandler$connectionHandler$getConnection(),
            cdmDatabaseSchema = cohortTableHandler$cdmDatabaseSchema,
            cohortTable = cohortTableHandler$cohortTableNames$cohortTable,
            vocabularyDatabaseSchema = cohortTableHandler$vocabularyDatabaseSchema,
            cohortIds = analysisSettings$databaseIdsCohorsIdsList[[databaseId]],
            # switches
            runInclusionStatistics = analysisSettings$runInclusionStatistics,
            runIncludedSourceConcepts = analysisSettings$runIncludedSourceConcepts,
            runOrphanConcepts = analysisSettings$runOrphanConcepts,
            runVisitContext = analysisSettings$runVisitContext,
            runIncidenceRate = analysisSettings$runIncidenceRate,
            runCohortRelationship = analysisSettings$runCohortRelationship,
            runTemporalCohortCharacterization = analysisSettings$runTemporalCohortCharacterization,
            runBreakdownIndexEvents = analysisSettings$runBreakdownIndexEvents,
            runTimeSeries = analysisSettings$runTimeSeries,
            #
            temporalCovariateSettings = analysisSettings$temporalCovariateSettings,
            #
            minCellCount = analysisSettings$minCellCount,
            incremental = FALSE
          )
        }

        ParallelLogger::logInfo("Results to csv")
        tmpdirTimeAnalysisResultsCsv <- file.path(tmpdirTime, "analysisResultsCsv")
        dir.create(tmpdirTimeAnalysisResultsCsv)
        HadesExtras::CohortDiagnostics_mergeCsvResults(
          pathToResultFolders = pathToResultFolders,
          pathToMergedRestulsFolder = tmpdirTimeAnalysisResultsCsv
        )

        yaml::write_yaml(analysisSettings, file.path(tmpdirTimeAnalysisResultsCsv, "analysisSettings.yaml"))

        analysisResultsZipCsvPath <- file.path(tmpdirTime, "analysisResultsCsv.zip")
        zip::zipr(zipfile = analysisResultsZipCsvPath, files = list.files(tmpdirTimeAnalysisResultsCsv, full.names = TRUE, recursive = TRUE))

        ParallelLogger::logInfo("Results to sqlite")
        tmpdirTimeAnalysisResultsSqlite <- file.path(tmpdirTime, "analysisResultsSqlite")
        dir.create(tmpdirTimeAnalysisResultsSqlite)

        CohortDiagnostics::createMergedResultsFile(
          dataFolder = tmpdirTime,
          sqliteDbPath = file.path(tmpdirTimeAnalysisResultsSqlite, "analysisResults.sqlite"),
          overwrite = TRUE
        )

        yaml::write_yaml(analysisSettings, file.path(tmpdirTimeAnalysisResultsSqlite, "analysisSettings.yaml"))

        analysisResultsZipSqlitePath <- file.path(tmpdirTime, "analysisResultsSqlite.zip")
        zip::zipr(zipfile = analysisResultsZipSqlitePath, files = list.files(tmpdirTimeAnalysisResultsSqlite, full.names = TRUE, recursive = TRUE))

        ParallelLogger::logInfo("End cohortDiagnostics")


        return(tmpdirTime)
      },
    .r_l = .r_l,
    logger = shiny::getShinyOption("logger"))


    #
    # display results
    #
    output$summary_text <- shiny::renderText({
      rf_results()$result
      shiny::req(rf_results)

      resultMessage  <- NULL
      if(rf_results()$success){
        #parse running muntes to a string with munutes and seconds
        runningTimeMinsSecs <- paste0(
          floor(rf_results()$runningTimeMins), " minutes and ",
          round((rf_results()$runningTimeMins-floor(rf_results()$runningTimeMins))*60), " seconds"
        )
        resultMessage <- paste0("✅ Success\n",
                                "🕒 Running time: ", runningTimeMinsSecs, "\n",
                                "📂 Results in: ", rf_results()$result)
        shiny::removeModal()
      }else{
        resultMessage <- paste0("❌ Error\n",
                                "📄 Message: ", rf_results()$result)
      }

      resultMessage
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
      filename = function(){"analysisName_cohortDiagnostics.zip"},
      content = function(fname){

        if(rf_results()$success){
          file.copy(file.path(rf_results()$result, "analysisResultsCsv.zip"), fname)
        }

        return(fname)
      }
    )


    shiny::observeEvent(input$view_actionButton, {
      shiny::req(rf_results())
      shiny::req(rf_results()$success)
      # open tab to url
      url <- paste0(shiny::getShinyOption("cohortOperationsConfig")$urlCohortOperationsViewer,
                    file.path(rf_results()$result, "analysisResultsSqlite.zip"))

      # run js code in client
      shinyjs::runjs(paste0("window.open('", url, "')"))

    })



  })
}














