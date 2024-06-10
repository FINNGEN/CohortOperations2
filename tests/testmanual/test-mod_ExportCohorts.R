# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

fcr_setUpLogger()

databasesHandlers <- helper_createNewDatabaseHandlers(withEunomiaCohorts = TRUE)

cohortsSummaryDatabases <- fct_getCohortsSummariesFromDatabasesHandlers(databasesHandlers)

r_connectionHandlers <- shiny::reactiveValues(
  databasesHandlers = databasesHandlers
)

r_workbench <- shiny::reactiveValues(
  cohortsSummaryDatabases = cohortsSummaryDatabases
)

# export module ----------------------------------------------------------------
devtools::load_all(".")

shiny::shinyApp(
  shiny::fluidPage(
    mod_cohortWorkbench_ui("test"),
    mod_exportsCohorts_ui("test")
  ),
  function(input,output,session){
    mod_cohortWorkbench_server("test", r_connectionHandlers, r_workbench)
    mod_exportsCohorts_server("test", r_connectionHandlers, r_workbench)
  },
  options = list(launch.browser=TRUE)
)

