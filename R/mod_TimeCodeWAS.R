


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
    mod_fct_covariateSelector_ui(
      inputId = ns("features_pickerInput"),
      label = "Select features to compare between cases and controls:",
      analysisIdsToShow = c(101, 102, 141, 204, 601, 641, 301, 341, 404, 906, 701, 741, 801, 841, 501, 541),
      analysisIdsSelected = c(101, 102, 204, 601, 301, 404, 701, 801, 501 )
    ),
    shiny::tags$h5("Minimum cell count:"),
    shiny::numericInput(
      inputId = ns("minCellCount_numericInput"),
      label = NULL,
      value = 1,
      min = 1,
      max = 1000
    ),
    htmltools::hr(),
    shiny::tags$h4("Time windows"),
    mod_formTimeWindows_ui(ns("selectRanges")),
    shiny::tags$br(),
    #
    htmltools::hr(),
    shiny::tags$h4("Pre-ran info"),
    shiny::verbatimTextOutput(ns("info_text"), placeholder = TRUE),
    shiny::tags$br(),
    shiny::actionButton(ns("run_actionButton"), "Run Study"),
    #
    htmltools::hr(),
    shiny::tags$h4("Results"),
    shiny::verbatimTextOutput(ns("results_text")),
    shiny::tags$br(),
    shiny::downloadButton(ns("download_actionButton"), "Download to Sandbox"),
    #shiny::downloadButton(ns("download_actionButton2"), "Download out of Sandbox"),
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
      cohortIdAndNamesList <- list()
      if(nrow(cohortIdAndNames) != 0){
        cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, paste(cohortIdAndNames$shortName, "➖"  , cohortIdAndNames$cohortName)))
      }
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
        cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, paste(cohortIdAndNames$shortName, "➖"  , cohortIdAndNames$cohortName)))
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
      #shinyjs::toggleState("features_pickerInput", condition = condition )
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
      shiny::req(input$features_pickerInput)
      shiny::req(r_ranges()$temporalStartDays)
      shiny::req(r_ranges()$temporalEndDays)
      shiny::req(input$minCellCount_numericInput)

      analysisSettings <- list(
        analysisType = "timeCodeWAS",
        cohortIdCases = input$selectCaseCohort_pickerInput,
        cohortIdControls = input$selectControlCohort_pickerInput,
        analysisIds = input$features_pickerInput,
        temporalStartDays = r_ranges()$temporalStartDays,
        temporalEndDays =   r_ranges()$temporalEndDays,
        minCellCount = input$minCellCount_numericInput
      )

      r$analysisSettings <- analysisSettings

    })

    output$info_text <- shiny::renderText({
      shiny::req(r_workbench$cohortsSummaryDatabases)
      shiny::req(input$selectDatabases_pickerInput)
      shiny::req(input$selectCaseCohort_pickerInput)
      shiny::req(input$selectControlCohort_pickerInput)

      cohortsOverlap <- r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandler$getCohortsOverlap()
      cohortCounts <-  r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandler$getCohortCounts()
      nSubjectsOverlap <- cohortsOverlap |>
        dplyr::filter(
          stringr::str_detect(cohortIdCombinations, paste0("-", input$selectCaseCohort_pickerInput, "-")) &
            stringr::str_detect(cohortIdCombinations, paste0("-", input$selectControlCohort_pickerInput, "-"))
        ) |>
        dplyr::pull(numberOfSubjects)  |>
        sum()
      nSubjectsCase <- cohortCounts |>
        dplyr::filter(cohortId == input$selectCaseCohort_pickerInput) |>
        dplyr::pull(cohortSubjects)
      nSubjectsControl <- cohortCounts |>
        dplyr::filter(cohortId == input$selectControlCohort_pickerInput) |>
        dplyr::pull(cohortSubjects)

      nEntryCase <- cohortCounts |>
        dplyr::filter(cohortId == input$selectCaseCohort_pickerInput) |>
        dplyr::pull(cohortEntries)
      nEntryControl <- cohortCounts |>
        dplyr::filter(cohortId == input$selectControlCohort_pickerInput) |>
        dplyr::pull(cohortEntries)


      cohortsSumary  <- r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandler$getCohortsSummary()

      message <- ""
      # counts
      if( nSubjectsCase > nSubjectsControl ){
        message <- paste0(message, "There are more entries in case cohort (", nEntryCase,") that in control cohort (", nEntryControl,"). Are you sure they are correct?\n")
      }

      # overlap
      if(nSubjectsOverlap==0){
        message <- paste0(message, "✅ No subjects overlap between case and control cohorts\n")
      }else{
        if(nSubjectsOverlap > nSubjectsCase * .20){
          message <- paste0(message, "❌ There are many subjects, ",nSubjectsOverlap, ", that overlap  berween case and control cohorts. Consider removing them in Operate Cohorts tab\n")
        }else{
          message <- paste0(message, "⚠️ There are few subjects, ",nSubjectsOverlap, ", that overlap between case and control cohorts. \n")
        }
      }

      # sex
      sexCase <- cohortsSumary |>
        dplyr::filter(cohortId == input$selectCaseCohort_pickerInput) |>
        dplyr::pull(sexCounts)
      sexControl <- cohortsSumary |>
        dplyr::filter(cohortId == input$selectControlCohort_pickerInput) |>
        dplyr::pull(sexCounts)
      nMaleCases <- sexCase[[1]]  |> dplyr::filter(sex == "MALE")  |> dplyr::pull(n)
      nMaleCases <- ifelse(length(nMaleCases)==0, 0, nMaleCases)
      nFemaleCases <- sexCase[[1]]  |> dplyr::filter(sex == "FEMALE")  |> dplyr::pull(n)
      nFemaleCases <- ifelse(length(nFemaleCases)==0, 0, nFemaleCases)
      nMaleControls <- sexControl[[1]]  |> dplyr::filter(sex == "MALE") |> dplyr::pull(n)
      nMaleControls <- ifelse(length(nMaleControls)==0, 0, nMaleControls)
      nFemaleControls <- sexControl[[1]]  |> dplyr::filter(sex == "FEMALE") |> dplyr::pull(n)
      nFemaleControls <- ifelse(length(nFemaleControls)==0, 0, nFemaleControls)

      data <-matrix(c(nMaleCases,nFemaleCases,nMaleControls,nFemaleControls),ncol=2)
      fisher_results <- stats::fisher.test(data)

      if(fisher_results$p.value < 0.05){
        message <- paste0(message, "⚠️ There is a significant difference in sex distribution between case and control cohorts. (Fisher's test p = ", scales::scientific(fisher_results$p.value)," ) \n")
        message <- paste0(message, "Consider controling for sex using regresion statistics or creating a new control cohort that match case cohort by sex in the Match Cohorts tab")
      }

      # year of birth
      yearOfBirthCase <- cohortsSumary |>
        dplyr::filter(cohortId == input$selectCaseCohort_pickerInput) |>
        dplyr::pull(histogramBirthYear)
      yearOfBirthControl <- cohortsSumary |>
        dplyr::filter(cohortId == input$selectControlCohort_pickerInput) |>
        dplyr::pull(histogramBirthYear)

      ttestResult <- t.test(yearOfBirthCase[[1]]  |> tidyr::uncount(n), yearOfBirthControl[[1]]  |> tidyr::uncount(n))

      if(ttestResult$p.value < 0.05){
        message <- paste0(message, "⚠️ There is a significant difference in year of birth distribution between case and control cohorts. (t-test p = ", scales::scientific(ttestResult$p.value)," ) \n")
        message <- paste0(message, "Consider controling for year of birth using regresion statistics or creating a new control cohort that match case cohort by year of birth in the Match Cohorts tab")
      }

      return(message)

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

      ParallelLogger::logInfo("[TimeCodeWAS] Run in database: ", input$selectDatabases_pickerInput, " with settings: ", .listToString(l))
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

        tmpdirTimeAnalysisResultsCsv <- file.path(tmpdirTime, "analysisResultsCsv")
        dir.create(tmpdirTimeAnalysisResultsCsv)

        HadesExtras::executeTimeCodeWAS(
          exportFolder = tmpdirTimeAnalysisResultsCsv,
          cohortTableHandler = cohortTableHandler,
          cohortIdCases = as.integer(analysisSettings$cohortIdCases),
          cohortIdControls = as.integer(analysisSettings$cohortIdControls),
          analysisIds = as.integer(analysisSettings$analysisIds),
          temporalStartDays = as.integer(analysisSettings$temporalStartDays),
          temporalEndDays = as.integer(analysisSettings$temporalEndDays),
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
          overwrite = TRUE
        )

        yaml::write_yaml(analysisSettings, file.path(tmpdirTimeAnalysisResultsSqlite, "analysisSettings.yaml"))

        analysisResultsZipSqlitePath <- file.path(tmpdirTime, "analysisResultsSqlite.zip")
        zip::zipr(zipfile = analysisResultsZipSqlitePath, files = list.files(tmpdirTimeAnalysisResultsSqlite, full.names = TRUE, recursive = TRUE))


        ParallelLogger::logInfo("End timeCodeWasCounts")

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
                                "🕒 Running time: ", runningTimeMinsSecs, "\n"
        )
        shiny::removeModal()
      }else{
        resultMessage <- paste0("❌ Error\n",
                                "📄 Message: ", rf_results()$result)
      }

      ParallelLogger::logInfo("[TimeCodeWAS] Ran results: ", resultMessage)

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
      filename = function(){"analysisName_timeCodeWAS.zip"},
      content = function(fname){
        if(rf_results()$success){
          file.copy(file.path(rf_results()$result, "analysisResultsSqlite.zip"), fname)
        }

        ParallelLogger::logInfo("[TimeCodeWAS] Download:")

        return(fname)
      }
    )


    shiny::observeEvent(input$view_actionButton, {
      ParallelLogger::logInfo("[TimeCodeWAS] Open in Viewer:")
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





















