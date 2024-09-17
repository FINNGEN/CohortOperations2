# build parameters --------------------------------------------------------------

devtools::load_all(".")
source(testthat::test_path("helper.R"))

fcr_setUpLogger()

testConfigFile <- "devatlas_databasesConfig.yml"

timestamp <- as.character(as.numeric(format(Sys.time(), "%d%m%Y%H%M%OS2"))*100)

test_databasesConfig <- readAndParseYalm(
  pathToYalmFile = testthat::test_path("config", testConfigFile),
  OAuthPvtKeyPath = Sys.getenv("GCP_SERVICE_KEY"),
  pathToDriver = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER"),
  timestamp = timestamp
)

test_cohortTableHandlerConfig <- test_databasesConfig[[1]]$cohortTableHandler

cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = NULL)

r_databaseConnection <- shiny::reactiveValues(
  cohortTableHandler = cohortTableHandler,
  atlasConfig = test_databasesConfig[[1]]$atlasConfig,
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



# test with ids 1,2,6
# Breast Cancer ICD10 Controls (id = 1) - copy cohort from prev generations
# Breast Cancer ICD10 Cases (id = 2) - copy cohort from prev generations
# Breast Cancer ICD10 Cases 2 (id = 6) - create new cohort


