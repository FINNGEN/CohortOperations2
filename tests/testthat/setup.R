# settings
if( Sys.getenv("EUNOMIA_DATA_FOLDER") == "" ){
  message("EUNOMIA_DATA_FOLDER not set. Please set this environment variable to the path of the Eunomia data folder.")
  stop()
}


#
# Databases config
#
databasesConfigFileName <- "eunomia_databasesConfig.yml"
#databasesConfigFileName <- "atlasDevelopment_cohortTableHandlerConfig.yml"
pathToDatabasesConfigYalm <- testthat::test_path("config", databasesConfigFileName)
databasesConfig <- yaml::read_yaml(pathToDatabasesConfigYalm)

# for each database, if using eunomia database create the file
for(databaseId in names(databasesConfig)){
  cohortTableHandlerConfig <- databasesConfig[[databaseId]]$cohortTableHandler
  if(!is.null(cohortTableHandlerConfig$connection$connectionDetailsSettings$server) &&
    cohortTableHandlerConfig$connection$connectionDetailsSettings$server == "Eunomia"){
    # create a new database and change the connection settings
    databaseName <- cohortTableHandlerConfig$connection$connectionDetailsSettings$databaseName
    eunomiaDatabaseFile  <- Eunomia::getDatabaseFile(databaseName, overwrite = FALSE)
    cohortTableHandlerConfig$connection$connectionDetailsSettings$server <- eunomiaDatabaseFile
    cohortTableHandlerConfig$connection$connectionDetailsSettings$databaseName <- NULL

    helper_addCohortAndCohortDefinitionTables(cohortTableHandlerConfig, cohortTablesToAdd = "Diabetes")

  }

  databasesConfig[[databaseId]]$cohortTableHandler <- cohortTableHandlerConfig
}

pathToDatabasesConfigYalm <- tempfile(fileext = ".yml")
databasesConfig |> yaml::write_yaml(pathToDatabasesConfigYalm)

# pick the one database to connect to
cohortTableHandlerConfig <- databasesConfig$E1$cohortTableHandler

#
# Modules config
#
analysisModulesConfigFileName <- "test_analysisModulesConfig.yml"
pathToAnalysisModulesConfigYalm <- testthat::test_path("config", analysisModulesConfigFileName)
analysisModulesConfig <- yaml::read_yaml(pathToAnalysisModulesConfigYalm)


# inform user
message("******* TESTING SETTINGS")
message("******* Databases config file: ", databasesConfigFileName, " Database: ", cohortTableHandlerConfig$database$databaseName)
message("******* Modules config: ", analysisModulesConfigFileName)
