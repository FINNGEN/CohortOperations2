


mod_codeWAS_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyjs::useShinyjs(),
    #
    shiny::tags$h4("Database"),
    shinyWidgets::pickerInput(
      inputId = ns("selectDatabases_pickerInput"),
      label = "Select database where calculate codeWAS:",
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
      analysisIdsToShow = c(101, 102, 141, 204, 1, 2, 3, 6, 8, 9, 10, 41, 601, 641, 301, 341, 404, 906, 701, 702, 703, 741, 908, 801, 841, 909,501, 541, 907, 910   ),
      analysisIdsSelected = c(141, 1, 2, 8, 10, 41, 641, 341, 404, 741, 841, 541 )
    ),
    shinyWidgets::radioGroupButtons(
      inputId = ns("statistics_type_option"),
      label = "Select comparison statistics:",
      choices = list(
        `Fisher's exact test and Welch's t-test (fast, no control for confounding)` = "aggregated",
        `(BETA) Logistic and linear regressions (slow, control for confounding)` = "full"
      ),
      direction = "vertical",
      individual = TRUE,
      checkIcon = list(
        yes = shiny::tags$i(class = "fa fa-circle",
                            style = "color: steelblue"),
        no = shiny::tags$i(class = "fa fa-circle-o",
                           style = "color: steelblue"))
    ),
    # conditional panel
    shiny::conditionalPanel(
      condition = "input.statistics_type_option == 'full'",
      ns = ns,
      shiny::tags$h5("Confounding variables: "),
      shiny::checkboxInput(
        inputId = ns("controlSex_checkboxInput"),
        label = "Sex",
        value = TRUE
      ),
      shiny::checkboxInput(
        inputId = ns("controlYearOfBirth_checkboxInput"),
        label = "Year of birth",
        value = TRUE
      ),
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


mod_codeWAS_server <- function(id, r_connectionHandlers, r_workbench) {
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

    rf_codeWASCounts <- shiny::reactiveVal()

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
      #shinyjs::toggleState("selectCovariates", condition = condition )
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
      shiny::req(input$minCellCount_numericInput)

      # if covariates selected, also add the necessary analysis
      covariatesIds <- NULL
      analysisIds  <-  input$features_pickerInput |> as.numeric()
      if(input$statistics_type_option == "full" & input$controlSex_checkboxInput){
        covariatesIds <- c(covariatesIds, 8507001)
        analysisIds <- union(analysisIds, 1)
      }
      if(input$statistics_type_option == "full" & input$controlYearOfBirth_checkboxInput){
        covariatesIds <- c(covariatesIds, 1041)
        analysisIds <- union(analysisIds, 41)
      }

      analysisSettings <- list(
        analysisType = "codeWAS",
        cohortIdCases = input$selectCaseCohort_pickerInput,
        cohortIdControls = input$selectControlCohort_pickerInput,
        analysisIds = analysisIds,
        covariatesIds = covariatesIds,
        minCellCount = input$minCellCount_numericInput,
        cores = cores,
        chunksSizeNOutcomes = chunksSizeNOutcomes
      )

      for(covaraiteSetting in input$selectCovariates){
        analysisSettings$temporalCovariateSettings[[covaraiteSetting]] <- TRUE
      }

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

      cohortsSumary  <- r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandler$getCohortsSummary()

      # cores
      message <- paste0("🔢 Analysis will use : ", cores, " cores\n")

      # counts
      if( nSubjectsCase > nSubjectsControl ){
        message <- paste0(message, "❌ There are more subjects in  case cohort (", nSubjectsCase,") that in control cohort (", nSubjectsControl,"). Are you sure they are correct?\n")
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
      if(!(input$statistics_type_option == "full" & input$controlSex_checkboxInput)){
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
          message <- paste0(message, "Consider controlling for sex using regression statistics or creating a new control cohort that match case cohort by sex in the Match Cohorts tab")
        }
      }

      # year of birth
      if(!(input$statistics_type_option == "full" & input$controlYearOfBirth_checkboxInput)){
        yearOfBirthCase <- cohortsSumary |>
          dplyr::filter(cohortId == input$selectCaseCohort_pickerInput) |>
          dplyr::pull(histogramBirthYear)
        yearOfBirthControl <- cohortsSumary |>
          dplyr::filter(cohortId == input$selectControlCohort_pickerInput) |>
          dplyr::pull(histogramBirthYear)

        ttestResult <- t.test(yearOfBirthCase[[1]]  |> tidyr::uncount(n), yearOfBirthControl[[1]]  |> tidyr::uncount(n))

        if(ttestResult$p.value < 0.05){
          message <- paste0(message, "⚠️ There is a significant difference in year of birth distribution between case and control cohorts. (t-test p = ", scales::scientific(ttestResult$p.value)," ) \n")
          message <- paste0(message, "Consider controlling for year of birth using regression statistics or creating a new control cohort that match case cohort by year of birth in the Match Cohorts tab")
        }
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

      ParallelLogger::logInfo("[CodeWAS] Run in database: ", input$selectDatabases_pickerInput, " with settings: ", .listToString(l))
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

        HadesExtras::executeCodeWAS(
          exportFolder = tmpdirTimeAnalysisResultsCsv,
          cohortTableHandler = cohortTableHandler,
          cohortIdCases = as.integer(analysisSettings$cohortIdCases),
          cohortIdControls = as.integer(analysisSettings$cohortIdControls),
          analysisIds = analysisSettings$analysisId,
          covariatesIds = analysisSettings$covariatesIds,
          minCellCount = analysisSettings$minCellCount,
          cores = analysisSettings$cores,
          chunksSizeNOutcomes = analysisSettings$chunksSizeNOutcomes
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
          analysis = "codeWAS"
        )

        yaml::write_yaml(analysisSettings, file.path(tmpdirTimeAnalysisResultsSqlite, "analysisSettings.yaml"))

        analysisResultsZipSqlitePath <- file.path(tmpdirTime, "analysisResultsSqlite.zip")
        zip::zipr(zipfile = analysisResultsZipSqlitePath, files = list.files(tmpdirTimeAnalysisResultsSqlite, full.names = TRUE, recursive = TRUE))


        ParallelLogger::logInfo("End codeWASCounts")

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
                                "🕒 Running time: ", runningTimeMinsSecs, "\n"
        )
        shiny::removeModal()
      }else{
        resultMessage <- paste0("❌ Error\n",
                                "📄 Message: ", rf_results()$result)
      }

      ParallelLogger::logInfo("[CodeWAS] Ran results: ", resultMessage)

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
      filename = function(){"analysisName_codeWAS.zip"},
      content = function(fname){
        if(rf_results()$success){
          file.copy(file.path(rf_results()$result, "analysisResultsSqlite.zip"), fname)
        }
        ParallelLogger::logInfo("[CodeWAS] Download:")
        return(fname)
      }
    )


    shiny::observeEvent(input$view_actionButton, {
      ParallelLogger::logInfo("[CodeWAS] Open in Viewer:")
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





















