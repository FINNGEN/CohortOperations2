# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

fcr_setUpLogger()

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
    mod_importCohortsFromFile_ui("test")
  ),
  function(input,output,session){
    mod_importCohortsFromFile_server("test", r_connectionHandlers, r_workbench)
    mod_cohortWorkbench_server("test", r_connectionHandlers, r_workbench)
  },
  options = list(launch.browser=TRUE)
)

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
