# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

fcr_setUpLogger()

cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "EunomiaDefaultCohorts")

r_databaseConnection <- shiny::reactiveValues(
  cohortTableHandler = cohortTableHandler,
  atlasConfig = NULL,
  hasChangeCounter = 0
)

# run module --------------------------------------------------------------
devtools::load_all(".")

# set module
selectedAnalysisModulesConfig <- analysisModulesConfig[["cohortOverlaps_CO2AnalysisModules"]]

analysisName  <- selectedAnalysisModulesConfig$analysisName
mod_analysisSettings_ui  <- selectedAnalysisModulesConfig$mod_analysisSettings_ui  |> fct_stringToFuction()
mod_analysisSettings_server  <- selectedAnalysisModulesConfig$mod_analysisSettings_server  |> fct_stringToFuction()
fct_executeAnalysis  <- selectedAnalysisModulesConfig$fct_executeAnalysis  |> fct_stringToFuction()
url_visualiseResults <- selectedAnalysisModulesConfig$url_visualiseResults

app <- shiny::shinyApp(
  shiny::fluidPage(
    mod_cohortWorkbench_ui("test"),
    mod_analysisWrap_ui("test", mod_analysisSettings_ui)
  ),
  function(input,output,session){
    mod_analysisWrap_server("test", r_databaseConnection, mod_analysisSettings_server, fct_executeAnalysis, analysisName, url_visualiseResults)
    mod_cohortWorkbench_server("test", r_databaseConnection)
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
