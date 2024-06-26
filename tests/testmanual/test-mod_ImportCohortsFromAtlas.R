# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

logger <- fcr_setUpLogger()

databasesHandlers <- helper_createNewDatabaseHandlers(withEunomiaCohorts = FALSE)

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
    mod_importCohortsFromAtlas_ui("test")
  ),
  function(input,output,session){
    mod_importCohortsFromAtlas_server("test", r_connectionHandlers, r_workbench, filterCohortsRegex='PENIS')
    mod_cohortWorkbench_server("test", r_connectionHandlers, r_workbench)
  },
  options = list(launch.browser=TRUE)
)

pathToCohortOperationsConfigYalm = testthat::test_path("config", "cohortOperationsConfig.yml")
cohortOperationsConfig <- yaml::read_yaml(pathToCohortOperationsConfigYalm)

app$appOptions$cohortOperationsConfig  <- cohortOperationsConfig
app$appOptions$logger  <- logger
app


# test with 1778211, 1778212, 1778213

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
