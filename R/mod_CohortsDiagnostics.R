


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
    htmltools::hr(),
    #
    shiny::tags$h4("Settings"),
    shiny::numericInput(
      inputId = ns("minCellCount_numericInput"),
      label = "Min Cell Count",
      value = 1,
      min = 1,
      max = 1000
    ),
    # TODO : add more setting for cohortDiagnostics
    htmltools::hr(),
    htmltools::h4("TODO : add more setting for cohortDiagnostics"),
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
    })

    #
    # create settings
    #
    shiny::observe({
      shiny::req(input$selectCohort_pickerInput)
      shiny::req(input$selectCohort_pickerInput!="NA")
      shiny::req(input$minCellCount_numericInput)

      # convert vector of strings databaseId-cohortId to tibble databaseId and cohortIds
      databaseIdsCohortIdsTibble<- data.frame(
        databaseId = gsub(pattern = "@.*", replacement = "", x = input$selectCohort_pickerInput),
        cohortId = gsub(pattern = ".*@", replacement = "", x = input$selectCohort_pickerInput)
      )

      databaseIdsCohorsIdsList <- list()
      for(databaseId in unique(databaseIdsCohortIdsTibble$databaseId)){
        databaseIdsCohorsIdsList[[databaseId]] <- databaseIdsCohortIdsTibble |> dplyr::filter(databaseId == !!databaseId) |> dplyr::pull(cohortId)
      }

      analysisSettings <- list(
        analysisType = "cohortDiagnostics",
        databaseIdsCohorsIdsList = databaseIdsCohorsIdsList,
        minCellCount = input$minCellCount_numericInput
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

            CohortDiagnostics:: executeDiagnostics(
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
              # runInclusionStatistics = FALSE,
              # runIncludedSourceConcepts = FALSE,
              # runOrphanConcepts = FALSE,
              # runTimeSeries = FALSE,
              # runVisitContext = FALSE,
              # runBreakdownIndexEvents = FALSE,
              # runIncidenceRate = TRUE,
              # runCohortRelationship = FALSE,
              # runTemporalCohortCharacterization = FALSE,
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
                                "🕒 Running time: ", runningTimeMinsSecs, " minutes\n",
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





















