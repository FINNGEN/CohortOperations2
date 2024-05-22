


mod_cohortsIncidence_ui <- function(id) {
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
    shiny::tags$h4("Settings"),
    shinyWidgets::pickerInput(
      inputId = ns("stratify_pickerInput"),
      label = "Select counts stratification:",
      choices = list(`Calendar Year`='calendarYear', `Age Group`='ageGroup', `Gender`="gender"),
      selected = list(`Calendar Year`='calendarYear', `Age Group`='ageGroup', `Gender`="gender"),
      multiple = TRUE),
    shinyWidgets::pickerInput(
      inputId = ns("references_pickerInput"),
      label = "Select counts refered to:",
      choices = list(`Cohort Onset`="cohort_start_date", `Cohort End`="cohort_end_date", `Birth`="birth_datetime"),
      selected = list(`Cohort Onset`="cohort_start_date", `Cohort End`="cohort_end_date", `Birth`="birth_datetime"),
      multiple = TRUE),
    shiny::numericInput(
      inputId = ns("minCellCount_numericInput"),
      label = "Min Cell Count",
      value = 1,
      min = 1,
      max = 1000
    ),
    htmltools::hr(),
    #
    htmltools::hr(),
    shiny::tags$h4("Run"),
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


mod_cohortsIncidence_server <- function(id, r_connectionHandlers, r_workbench) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns



    #
    # reactive variables
    #

    r <- shiny::reactiveValues(
      analysisSettings = NULL
    )

    rf_results <- shiny::reactiveVal()

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
    # activate settings if cohors have been selected
    #
    shiny::observe({
      condition <- !is.null(input$selectCohort_pickerInput)  && !is.null(input$minCellCount_numericInput) && !is.null(input$stratify_pickerInput) && !is.null(input$references_pickerInput)
      shinyjs::toggleState("run_actionButton", condition = condition )
    })

    #
    # create settings
    #
    shiny::observe({
      shiny::req(input$selectCohort_pickerInput)
      shiny::req(input$selectCohort_pickerInput!="NA")
      shiny::req(input$minCellCount_numericInput)
      shiny::req(input$stratify_pickerInput)
      shiny::req(input$references_pickerInput)

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
        studyType = "cohortsIncidence",
        databaseIdsCohorsIdsList = databaseIdsCohorsIdsList,
        minCellCount = input$minCellCount_numericInput,
        groupBy = input$stratify_pickerInput,
        referenceYears = input$references_pickerInput
        )

      r$analysisSettings <- analysisSettings

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

      ParallelLogger::logInfo("[Demographics] Run in databases: ", input$selectDatabases_pickerInput, " with settings: ", .listToString(l))
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

            HadesExtras::executeCohortDemographicsCounts(
              exportFolder = exportFolder,
              cohortTableHandler = cohortTableHandler,
              cohortIds = as.integer(analysisSettings$databaseIdsCohorsIdsList[[databaseId]]),
              referenceYears = analysisSettings$referenceYears,
              groupBy = analysisSettings$groupBy,
              minCellCount = analysisSettings$minCellCount
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

        HadesExtras::csvFilesToSqlite(
          dataFolder = tmpdirTimeAnalysisResultsCsv,
          sqliteDbPath = file.path(tmpdirTimeAnalysisResultsSqlite, "analysisResults.sqlite"),
          overwrite = TRUE,
          analysis = "cohortDemographics"
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
    output$results_text <- shiny::renderText({

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

      ParallelLogger::logInfo("[CohortDiagnostics] Ran results: ", resultMessage)

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
          file.copy(file.path(rf_results()$result, "analysisResultsSqlite.zip"), fname)
        }

        ParallelLogger::logInfo("[CohortDiagnostics] Download:")

        return(fname)
      }
    )


    shiny::observeEvent(input$view_actionButton, {
      ParallelLogger::logInfo("[CohortDiagnostics] Open in Viewer:")
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



































