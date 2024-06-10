

# Run the full app locally using eunomia

devtools::load_all(".")
run_app(
  pathToCohortOperationsConfigYalm = testthat::test_path("config", "cohortOperationsConfig.yml"),
  pathToDatabasesConfigYalm = testthat::test_path("config", "eunomia_databasesConfig.yml"),
  options = list(port = 9999, launch.browser = TRUE)
  )



# Run the full app locally using atlasDev bigquery

devtools::load_all(".")
run_app(
  pathToCohortOperationsConfigYalm = testthat::test_path("config", "cohortOperationsConfig.yml"),
  pathToDatabasesConfigYalm = testthat::test_path("config", "devatlas_databasesConfig.yml"),
  options = list(port = 9999, launch.browser = TRUE)
  )


# Run modules independently
# find the codes for each module in tests/testmanual/test-mod_<module>
