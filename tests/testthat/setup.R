#
# SELECT DATABASE and CO2 CONFIGURATION
#

# Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "Eunomia-GiBleed")
# Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "Eunomia-MIMIC")
# Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "Eunomia-FinnGen")
# Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "AtlasDevelopment-5k")
# Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "AtlasDevelopment-full")
testingDatabase <- Sys.getenv("HADESEXTAS_TESTING_ENVIRONMENT")

testingAnalysisModulesConfig  <- "CO2AnalysisModulesOnly"

# check correct settings
possibleDatabases <- c("Eunomia-GiBleed", "Eunomia-MIMIC", "Eunomia-FinnGen", "AtlasDevelopment-5k", "AtlasDevelopment-full")
if (!(testingDatabase %in% possibleDatabases)) {
  message("Please set a valid testing environment in envar HADESEXTAS_TESTING_ENVIRONMENT, from: ", paste(possibleDatabases, collapse = ", "))
  stop()
}

#
# Eunomia Databases
#
if (testingDatabase |> stringr::str_starts("Eunomia")) {
  if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") {
    message("EUNOMIA_DATA_FOLDER not set. Please set this environment variable to the path of the Eunomia data folder.")
    stop()
  }

  pathToGiBleedEunomiaSqlite <- Eunomia::getDatabaseFile("GiBleed", overwrite = FALSE)
  pathToMIMICEunomiaSqlite <- Eunomia::getDatabaseFile("MIMIC", overwrite = FALSE)

  test_databasesConfig <- HadesExtras::readAndParseYaml(
    pathToYalmFile = testthat::test_path("config", "databasesConfig.yml"),
    pathToGiBleedEunomiaSqlite = pathToGiBleedEunomiaSqlite,
    pathToMIMICEunomiaSqlite = pathToMIMICEunomiaSqlite,
    pathToFinnGenEunomiaSqlite = ""#helper_FinnGen_getDatabaseFile()
  )

  if (testingDatabase |> stringr::str_ends("GiBleed")) {
    test_cohortTableHandlerConfig <- test_databasesConfig[[1]]$cohortTableHandle
  }
  if (testingDatabase |> stringr::str_ends("MIMIC")) {
    test_cohortTableHandlerConfig <- test_databasesConfig[[2]]$cohortTableHandle
  }
  if (testingDatabase |> stringr::str_ends("FinnGen")) {
    test_cohortTableHandlerConfig <- test_databasesConfig[[4]]$cohortTableHandle
  }

  # add test cohorts and cohort definitions
  helper_addCohortAndCohortDefinitionTables(cohortTableHandlerConfig = test_cohortTableHandlerConfig)
}


#
# AtlasDevelopmet-DBI Database
#
if (testingDatabase |> stringr::str_starts("AtlasDevelopment")) {
  if (Sys.getenv("GCP_SERVICE_KEY") == "") {
    message("GCP_SERVICE_KEY not set. Please set this environment variable to the path of the GCP service key.")
    stop()
  }

  bigrquery::bq_auth(path = Sys.getenv("GCP_SERVICE_KEY"))

  test_databasesConfig <- HadesExtras::readAndParseYaml(
    pathToYalmFile = testthat::test_path("config", "databasesConfig.yml")
  )

  if (testingDatabase |> stringr::str_ends("5k")) {
    test_cohortTableHandlerConfig <- test_databasesConfig[[5]]$cohortTableHandler
  }
  if (testingDatabase |> stringr::str_ends("full")) {
    test_cohortTableHandlerConfig <- test_databasesConfig[[6]]$cohortTableHandler
  }
}

#
# CO2 Analysis Modules Configuration
#
if(testingAnalysisModulesConfig == "CO2AnalysisModulesOnly"){
  test_analysisModulesConfig <- HadesExtras::readAndParseYaml(
    pathToYalmFile = testthat::test_path("config", "analysisModulesConfig.yml")
  )
}
#
# INFORM USER
#
message("************* Testing on: ")
message("Database: ", testingDatabase)
message("CO2 Modules: ", testingAnalysisModulesConfig)


