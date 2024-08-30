# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

logger <- fcr_setUpLogger()

cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "EunomiaDefaultCohorts")

r_connectionHandler <- shiny::reactiveValues(
  cohortTableHandler = cohortTableHandler,
  hasChangeCounter = 0
)

# run module --------------------------------------------------------------
devtools::load_all(".")

shiny::shinyApp(
  shiny::fluidPage(
    mod_cohortWorkbench_ui("test")
  ),
  function(input,output,session){
    mod_cohortWorkbench_server("test", r_connectionHandler, table_editing=TRUE)
  },
  options = list(launch.browser=TRUE)
)

app$appOptions$logger  <- logger
app
