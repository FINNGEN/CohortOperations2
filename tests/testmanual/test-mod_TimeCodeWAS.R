

# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

future::plan(future::multisession, workers = 2)

folderWithLog <- file.path(tempdir(), "logs")
dir.create(folderWithLog, showWarnings = FALSE)
logger <- ParallelLogger::createLogger(
  appenders = list(
    # to console for traking
    ParallelLogger::createConsoleAppender(
      layout = .layoutParallelWithHeader
    ),
    # to file for showing in app
    ParallelLogger::createFileAppender(
      fileName = file.path(folderWithLog, "log.txt"),
      layout = ParallelLogger::layoutSimple
    )
  )
)
ParallelLogger::clearLoggers()
ParallelLogger::registerLogger(logger)
ParallelLogger::logTrace("Start logging")

shiny::addResourcePath("logs", folderWithLog)



databasesHandlers <- helper_createNewDatabaseHandlers(withEunomiaCohorts = TRUE)

cohortsSummaryDatabases <- fct_getCohortsSummariesFromDatabasesHandlers(databasesHandlers)

r_connectionHandlers <- shiny::reactiveValues(
  databasesHandlers = databasesHandlers
)

r_workbench <- shiny::reactiveValues(
  cohortsSummaryDatabases = cohortsSummaryDatabases
)

# run module --------------------------------------------------------------
devtools::load_all(".")

app <- shiny::shinyApp(
  shiny::fluidPage(
    mod_cohortWorkbench_ui("test"),
    mod_timeCodeWAS_ui("test")
  ),
  function(input,output,session){
    mod_cohortWorkbench_server("test", r_connectionHandlers, r_workbench)
    mod_timeCodeWAS_server("test", r_connectionHandlers, r_workbench)
  },
  options = list(launch.browser=TRUE)
)

app$appOptions$logger  <- logger
app


# connectionStatus_reactable ----------------------------------------------
# devtools::load_all(".")
#
# log <- HadesExtras::LogTibble$new()
# log$INFO("step 1", "example info")
# log$WARNING("step 2", "example warning")
# log$ERROR("step 3", "example error")
#
# connectionStatusLogs <- log$logTibble |> dplyr::mutate(database="Database name")
#
# connectionStatus_reactable(connectionStatusLogs)
