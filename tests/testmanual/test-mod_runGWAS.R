# build parameters --------------------------------------------------------------

devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

databasesHandlers <- helper_createNewDatabaseHandlers(withEunomiaCohorts = TRUE)
cohortsSummaryDatabases <- fct_getCohortsSummariesFromDatabasesHandlers(databasesHandlers)

connection_sandboxAPI <- list(
  base_url = "https://internal-api.app.finngen.fi/internal-api/",
  token = "",
  name = "",
  notification_email = "",
  conn_status_tibble = HadesExtras::LogTibble$new()
)

r_connectionHandlers <- shiny::reactiveValues(
  databasesHandlers = databasesHandlers,
  connection_sandboxAPI = connection_sandboxAPI
)

r_workbench <- shiny::reactiveValues(
  cohortsSummaryDatabases = cohortsSummaryDatabases
)

# export module ----------------------------------------------------------------
devtools::load_all(".")

shiny::shinyApp(
  shiny::fluidPage(
    mod_cohortWorkbench_ui("test"),
    mod_runGWAS_ui("test")
  ),
  function(input,output,session){
    mod_cohortWorkbench_server("test", r_connectionHandlers, r_workbench)
    mod_runGWAS_server("test", r_connectionHandlers, r_workbench)
  },
  options = list(launch.browser=TRUE)
)

