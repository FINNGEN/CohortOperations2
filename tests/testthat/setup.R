#
# SELECT DATABASE and CO2 CONFIGURATION
#
testingDatabase <- "Eunomia"
testingDatabase <- "AtlasDevelopment"
testingAnalysisModulesConfig <- "CO2AnalysisModulesOnly"

# check correct settings
possibleDatabases <- c("Eunomia", "AtlasDevelopment")
if( !(testingDatabase %in% possibleDatabases) ){
  message("Please select a valid database from: ", paste(possibleDatabases, collapse = ", "))
  stop()
}

possibleCO2AnalysisModulesConfig <- c("CO2AnalysisModulesOnly")
if( !(testingAnalysisModulesConfig %in% possibleCO2AnalysisModulesConfig) ){
  message("Please select a valid CO2 Analysis Modules configuration from: ", paste(possibleCO2AnalysisModulesConfig, collapse = ", "))
  stop()
}

#
# Eunomia Databases
#
if (testingDatabase == "Eunomia") {

  if( Sys.getenv("EUNOMIA_DATA_FOLDER") == "" ){
    message("EUNOMIA_DATA_FOLDER not set. Please set this environment variable to the path of the Eunomia data folder.")
    stop()
  }

  pathToGiBleedEunomiaSqlite  <- Eunomia::getDatabaseFile("GiBleed", overwrite = FALSE)
  pathToMIMICEunomiaSqlite  <- Eunomia::getDatabaseFile("MIMIC", overwrite = FALSE)

  test_databasesConfig <- readAndParseYalm(
    pathToYalmFile = testthat::test_path("config", "eunomia_databasesConfig.yml"),
    pathToGiBleedEunomiaSqlite = pathToGiBleedEunomiaSqlite,
    pathToMIMICEunomiaSqlite = pathToMIMICEunomiaSqlite
  )

  test_cohortTableHandlerConfig  <- test_databasesConfig[[1]]$cohortTableHandler
}


#
# AtlasDevelopmet Database
#
if (testingDatabase %in% c("AtlasDevelopment") ) {

  if( Sys.getenv("GCP_SERVICE_KEY") == "" ){
    message("GCP_SERVICE_KEY not set. Please set this environment variable to the path of the GCP service key.")
    stop()
  }

  if( Sys.getenv("DATABASECONNECTOR_JAR_FOLDER") == "" ){
    message("DATABASECONNECTOR_JAR_FOLDER not set. Please set this environment variable to the path of the database connector jar folder.")
    stop()
  }

  test_databasesConfig <- readAndParseYalm(
    pathToYalmFile = testthat::test_path("config", "devatlas_databasesConfig.yml"),
    OAuthPvtKeyPath = Sys.getenv("GCP_SERVICE_KEY"),
    pathToDriver = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
  )

  test_cohortTableHandlerConfig  <- test_databasesConfig[[1]]$cohortTableHandler
}

#
# CO2 Analysis Modules Configuration
#
if(testingAnalysisModulesConfig == "CO2AnalysisModulesOnly"){
  test_analysisModulesConfig <- readAndParseYalm(
    pathToYalmFile = testthat::test_path("config", "test_analysisModulesConfig.yml")
  )
}
#
# INFORM USER
#
message("************* Testing on: ")
message("Database: ", testingDatabase)
message("CO2 Modules: ", testingAnalysisModulesConfig)













