


mod_cohortOverlaps_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyjs::useShinyjs(),
    #
    shiny::tags$h4("Database"),
    shinyWidgets::pickerInput(
      inputId = ns("selectDatabases_pickerInput"),
      label = "Select database where calculate cohort overlaps:",
      choices = NULL,
      selected = NULL,
      multiple = FALSE),
    htmltools::hr(),
    shiny::tags$h4("Cohorts"),
    shinyWidgets::pickerInput(
      inputId = ns("selectCohorts_pickerInput"),
      label = "Select one or more cohorts:",
      choices = NULL,
      selected = NULL,
      multiple = TRUE),
    shiny::tags$h5("Minimum cell count:"),
    shiny::numericInput(
      inputId = ns("minCellCount_numericInput"),
      label = NULL,
      value = 1,
      min = 1,
      max = 1000
    ),
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


mod_cohortOverlaps_server <- function(id, r_connectionHandlers, r_workbench) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    cores <- shiny::getShinyOption("cores")
    chunksSizeNOutcomes <- shiny::getShinyOption("chunksSizeNOutcomes")

    #
    # reactive variables
    #
    r_ranges <- mod_formTimeWindows_server("selectRanges")

    r <- shiny::reactiveValues(
      analysisSettings = NULL
    )

    rf_cohortOverlapsCounts <- shiny::reactiveVal()

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
    # update selectCohorts_pickerInput with cohort names in selectDatabases_pickerInput database
    #
    shiny::observe({
      shiny::req(r_workbench$cohortsSummaryDatabases)
      shiny::req(input$selectDatabases_pickerInput)

      cohortIdAndNames <- r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandler$getCohortIdAndNames()
      cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, cohortIdAndNames$cohortName))

      shinyWidgets::updatePickerInput(
        inputId = "selectCohorts_pickerInput",
        choices = cohortIdAndNamesList,
        selected = cohortIdAndNamesList
      )
    })


    #
    # activate settings if cohors have been selected
    #
    shiny::observe({
      condition <- !is.null(input$selectCohorts_pickerInput) & input$selectCohorts_pickerInput!="NA"
      #shinyjs::toggleState("selectCovariates", condition = condition )
      shinyjs::toggleState("run_actionButton", condition = condition )
    })

    #
    # create settings
    #
    shiny::observe({
      shiny::req(input$selectDatabases_pickerInput)
      shiny::req(input$selectCohorts_pickerInput)
      shiny::req(input$selectCohorts_pickerInput!="NA")
      shiny::req(input$minCellCount_numericInput)

      analysisSettings <- list(
        analysisType = "cohortOverlaps",
        cohortIds = input$selectCohorts_pickerInput,
        minCellCount = input$minCellCount_numericInput
      )

      r$analysisSettings <- analysisSettings

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

      ParallelLogger::logInfo("[cohortOverlaps] Run in database: ", input$selectDatabases_pickerInput, " with settings: ", .listToString(l))
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
        tmpdirTimeAnalysisResultsCsv <- file.path(tmpdirTime, "analysisResultsCsv")
        dir.create(tmpdirTimeAnalysisResultsCsv)

        HadesExtras::executeCohortOverlaps(
          exportFolder = tmpdirTimeAnalysisResultsCsv,
          cohortTableHandler = cohortTableHandler,
          cohortIds = as.integer(analysisSettings$cohortIds),
          minCellCount = analysisSettings$minCellCount
        )

        ParallelLogger::logInfo("Results to csv")
        yaml::write_yaml(tmpdirTimeAnalysisResultsCsv, file.path(tmpdirTime, "analysisSettings.yaml"))

        analysisResultsZipCsvPath <- file.path(tmpdirTime, "analysisResultsCsv.zip")
        zip::zipr(zipfile = analysisResultsZipCsvPath, files = list.files(tmpdirTimeAnalysisResultsCsv, full.names = TRUE, recursive = TRUE))

        ParallelLogger::logInfo("Results to sqlite")
        tmpdirTimeAnalysisResultsSqlite <- file.path(tmpdirTime, "analysisResultsSqlite")
        dir.create(tmpdirTimeAnalysisResultsSqlite)

        HadesExtras::csvFilesToSqlite(
          dataFolder = tmpdirTimeAnalysisResultsCsv,
          sqliteDbPath = file.path(tmpdirTimeAnalysisResultsSqlite, "analysisResults.sqlite"),
          overwrite = TRUE,
          analysis = "cohortOverlaps"
        )

        yaml::write_yaml(analysisSettings, file.path(tmpdirTimeAnalysisResultsSqlite, "analysisSettings.yaml"))

        analysisResultsZipSqlitePath <- file.path(tmpdirTime, "analysisResultsSqlite.zip")
        zip::zipr(zipfile = analysisResultsZipSqlitePath, files = list.files(tmpdirTimeAnalysisResultsSqlite, full.names = TRUE, recursive = TRUE))


        ParallelLogger::logInfo("End cohortOverlapsCounts")

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

      ParallelLogger::logInfo("[cohortOverlaps] Ran results: ", resultMessage)

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
      filename = function(){"analysisName_cohortOverlaps.zip"},
      content = function(fname){
        if(rf_results()$success){
          file.copy(file.path(rf_results()$result, "analysisResultsSqlite.zip"), fname)
        }
        ParallelLogger::logInfo("[cohortOverlaps] Download:")
        return(fname)
      }
    )


    shiny::observeEvent(input$view_actionButton, {
      ParallelLogger::logInfo("[cohortOverlaps] Open in Viewer:")
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





















