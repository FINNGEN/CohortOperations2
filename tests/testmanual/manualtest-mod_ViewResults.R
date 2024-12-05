# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

fcr_setUpLogger()

analysisModulesConfig <- readr::read_yaml(testthat::test_path("config/test_analysisModulesConfig.yml"))

# run module --------------------------------------------------------------
devtools::load_all(".")

app <- shiny::shinyApp(
  shiny::fluidPage(
    mod_viewResults_ui("test")
  ),
  function(input, output, session) {
    shiny::shinyOptions(analysisModulesConfig = analysisModulesConfig)
    mod_viewResults_server("test")
  },
  options = list(launch.browser = TRUE)
)

app 
