# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

logger <- fcr_setUpLogger()

cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "EunomiaDefaultCohorts")

# uncomment to test missing connection to table
# cohortTableHandler$connectionHandler$getConnection() |> DatabaseConnector::dbExecute("DROP TABLE cohort")
# cohortTableHandler$connectionHandler$getConnection() |> DatabaseConnector::dbExecute("DROP TABLE cohort_definition")

r_connectionHandler <- shiny::reactiveValues(
  cohortTableHandler = cohortTableHandler,
  hasChangeCounter = 0
)

# run module --------------------------------------------------------------
devtools::load_all(".")

app <- shiny::shinyApp(
  shiny::fluidPage(
    mod_cohortWorkbench_ui("test"),
    mod_importCohortsFromCohortsTable_ui("test")
  ),
  function(input,output,session){
    mod_importCohortsFromCohortsTable_server("test", r_connectionHandler)
    mod_cohortWorkbench_server("test", r_connectionHandler)
  },
  options = list(launch.browser=TRUE)
)

app$appOptions$logger  <- logger
app

