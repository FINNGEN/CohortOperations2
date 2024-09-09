# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

fcr_setUpLogger()

cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "EunomiaDefaultCohorts")

r_databaseConnection <- shiny::reactiveValues(
  cohortTableHandler = cohortTableHandler,
  atlasConfig = list(
    webapiurl = "https://api.ohdsi.org/WebAPI"
  ),
  hasChangeCounter = 0
)

# run module --------------------------------------------------------------
devtools::load_all(".")

app <- shiny::shinyApp(
  shiny::fluidPage(
    mod_cohortWorkbench_ui("test"),
    mod_importCohortsFromAtlas_ui("test")
  ),
  function(input,output,session){
    mod_importCohortsFromAtlas_server("test", r_databaseConnection)
    mod_cohortWorkbench_server("test", r_databaseConnection)
  },
  options = list(launch.browser=TRUE)
)


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
