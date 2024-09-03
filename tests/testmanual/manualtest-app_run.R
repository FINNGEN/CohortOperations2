# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))


# run app --------------------------------------------------------------
devtools::load_all(".")
run_app(
  pathToDatabasesConfigYalm = pathToDatabasesConfigYalm, # from setup
  pathToAnalysisModulesConfigYalm = pathToAnalysisModulesConfigYalm, # from setup
  options = list(launch.browser=TRUE)
  )
