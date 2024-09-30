# Set up
devtools::load_all(".")
source(testthat::test_path("setup.R"))


# Run the full app locally using eunomiadevtools::load_all(".")
devtools::load_all(".")
app <- run_app(
  databasesConfig = test_databasesConfig,
  analysisModulesConfig = test_analysisModulesConfig,
  options = list(port = 9999, launch.browser = TRUE)
)

app
