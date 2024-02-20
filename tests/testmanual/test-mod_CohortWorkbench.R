# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))


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

shiny::shinyApp(
  shiny::fluidPage(
    mod_cohortWorkbench_ui("test")
  ),
  function(input,output,session){
    mod_cohortWorkbench_server("test", r_connectionHandlers, r_workbench)
  }
)

