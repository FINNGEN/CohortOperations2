# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

fcr_setUpLogger()

cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")

r_connectionHandler <- shiny::reactiveValues(
  cohortTableHandler = cohortTableHandler,
  hasChangeCounter = 0
)

# run module --------------------------------------------------------------
devtools::load_all(".")

app <- shiny::shinyApp(
  shiny::fluidPage(
    mod_cohortWorkbench_ui("test"),
    mod_exportsCohorts_ui("test")
  ),
  function(input,output,session){
    mod_exportsCohorts_server("test", r_connectionHandler)
    mod_cohortWorkbench_server("test", r_connectionHandler)
  },
  options = list(launch.browser=TRUE)
)


app
