# settings
if( Sys.getenv("EUNOMIA_DATA_FOLDER") == "" ){
  message("EUNOMIA_DATA_FOLDER not set. Please set this environment variable to the path of the Eunomia data folder.")
  stop()
}

testConfigFile <- "eunomia_databasesConfig.yml"
#testConfigFile <- "atlasDevelopment_cohortTableHandlerConfig.yml"
databasesConfig <- yaml::read_yaml(testthat::test_path("config", testConfigFile))


# for each database, if using eunomia database create the file
for(databaseId in names(databasesConfig)){
  cohortTableHandlerConfig <- databasesConfig[[databaseId]]$cohortTableHandler
  if(!is.null(cohortTableHandlerConfig$connection$connectionDetailsSettings$server) &&
    cohortTableHandlerConfig$connection$connectionDetailsSettings$server == "Eunomia"){
    databaseName <- cohortTableHandlerConfig$connection$connectionDetailsSettings$databaseName
    eunomiaDatabaseFile  <- Eunomia::getDatabaseFile(databaseName, overwrite = FALSE)
    cohortTableHandlerConfig$connection$connectionDetailsSettings$server <- eunomiaDatabaseFile
    cohortTableHandlerConfig$connection$connectionDetailsSettings$databaseName <- NULL
  }
  databasesConfig[[databaseId]]$cohortTableHandler <- cohortTableHandlerConfig
}

# pick the one database to connect to
cohortTableHandlerConfig <- databasesConfig$E1$cohortTableHandler


# inform user
message("************* Testing on Config file: ", testConfigFile, " Database: ", cohortTableHandlerConfig$database$databaseName, "*************")

