# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

fcr_setUpLogger()

cohortTableHandler <- helper_createNewCohortTableHandler()



# run module --------------------------------------------------------------
devtools::load_all(".")
 
app <- shiny::shinyApp(
  shiny::fluidPage(
    mod_cohortWorkbench_ui("teste"),
    mod_importCohortsFromFile_ui("test")
  ),
  function(input, output, session) {
    r_databaseConnection <- shiny::reactiveValues(
      cohortTableHandler = cohortTableHandler,
      atlasConfig = list(
        regexPersonSourceValue = "^00[0-9a-f]{6}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
      ),
      hasChangeCounter = 0
    )

    mod_cohortWorkbench_server("teste", r_databaseConnection)
    mod_importCohortsFromFile_server("test", r_databaseConnection)
  },
  options = list(launch.browser = TRUE)
)


app

