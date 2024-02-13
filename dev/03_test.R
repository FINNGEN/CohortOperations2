# # Modules
#
# ##mod_cohorts_table
# - stand alone testing
# - NOTES: cant be module tested because I cant change `reactable::getReactableState`
#
# ##mod_connection_to_db
# - Server function test
# - reload button
# - stand alone testing
#
# ##mod_import_cohort_atlas
# - Server function test
# - stand alone testing
#
# ##mod_import_cohort_file
# - Server function test
# - stand alone testing


devtools::load_all(".")
run_app(
  pathToCohortOperationsConfigYalm = testthat::test_path("config", "cohortOperationsConfig.yml"),
  pathToDatabasesConfigYalm = testthat::test_path("config", "eunomia_databasesConfig.yml"),
  options = list(port = 8080, launch.browser = TRUE)
  )
