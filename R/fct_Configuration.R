


fct_getDatabaseIdNamesListFromdatabasesConfig <- function(databasesConfig) {

  fct_assertdatabasesConfig(databasesConfig)

  databaseIdNamesList <- list()
  for(databaseId in names(databasesConfig)){
    databaseIdNamesList[[databasesConfig[[databaseId]]$cohortTableHandler$database$databaseName]] <- databaseId
  }

  return(databaseIdNamesList)

}




fct_databasesConfigToDatabasesHandlers <- function(
    databasesConfig,
    loadConnectionChecksLevel = "basicChecks") {

  fct_assertdatabasesConfig(databasesConfig)

  databasesHandlers <- list()
  for(databaseId in names(databasesConfig)){

    cohortTableHandlerConfig <- databasesConfig[[databaseId]]$cohortTableHandler

    cohortTableHandler <- HadesExtras::createCohortTableHandlerFromList(cohortTableHandlerConfig, loadConnectionChecksLevel)

    # TEMP
    if(TRUE){
      testCohortTable <-  tibble::tribble(
        ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
        1, 1, as.Date("2000-01-01"), as.Date("2000-12-01"),
        1, 2, as.Date("2000-01-01"), as.Date("2000-12-01"),
        1, 3, as.Date("2000-01-01"), as.Date("2000-12-01"),
        1, 4, as.Date("2000-01-01"), as.Date("2000-12-01"),
        1, 5, as.Date("2000-01-01"), as.Date("2000-12-01"),
        1, 5, as.Date("2004-01-01"), as.Date("2004-12-01"),

        2, 2, as.Date("2001-01-01"), as.Date("2002-12-01"),# non overplaping
        2, 3, as.Date("2000-06-01"), as.Date("2000-09-01"),# inside
        2, 4, as.Date("2000-06-01"), as.Date("2010-12-01"),# overlap
        2, 5, as.Date("2004-06-01"), as.Date("2010-12-01"),# overlap with second
        2, 6, as.Date("2000-01-01"), as.Date("2010-12-01")
      )

      testCohortDefinitionTable <- tibble::tribble(
        ~cohort_definition_id, ~cohort_definition_name, ~cohort_definition_description, ~definition_type_concept_id, ~cohort_definition_syntax, ~subject_concept_id, ~cohort_initiation_date,
        1, 'Diabetes Cohort', 'Cohort of patients diagnosed with diabetes', 1234, 'SELECT * FROM patients WHERE diagnosis = "Diabetes"', 5678, as.Date('2022-01-01'),
        2, 'Hypertension Cohort', 'Cohort of patients diagnosed with hypertension', 1234, 'SELECT * FROM patients WHERE diagnosis = "Hypertension"', 5678, as.Date('2022-01-01'),
        3, 'Obesity Cohort', 'Cohort of patients diagnosed with obesity', 1234, 'SELECT * FROM patients WHERE diagnosis = "Obesity"', 5678, as.Date('2022-01-01')
      )

      DatabaseConnector::insertTable(
        connection = cohortTableHandler$connectionHandler$getConnection(),
        table = "cohort",
        data = testCohortTable
      )

      DatabaseConnector::insertTable(
        connection = cohortTableHandler$connectionHandler$getConnection(),
        table = "cohort_definition",
        data = testCohortDefinitionTable
      )
    }

    databasesHandlers[[databaseId]] <- list(cohortTableHandler = cohortTableHandler)
  }

  return(databasesHandlers)

}



fct_checkdatabasesConfig  <- function(databasesConfig) {
  collection <- .fct_assertdatabasesConfig(databasesConfig)
  if (collection$isEmpty()) {
    return(TRUE)
  } else {
    return(collection$getMessages())
  }
}

fct_assertdatabasesConfig  <- function(databasesConfig) {
  collection <- .fct_assertdatabasesConfig(databasesConfig)
  if (!collection$isEmpty()) {
    checkmate::reportAssertions(collection)
  }
}

.fct_assertdatabasesConfig <- function(databasesConfig) {

  collection = checkmate::makeAssertCollection()

  databasesConfig |> checkmate::assertList()
  # TODO check structure

  return(collection)

}

fcr_setUpLogger  <- function(){
  folderWithLog <- file.path(tempdir(), "logs")
  dir.create(folderWithLog, showWarnings = FALSE)
  logger <- ParallelLogger::createLogger(
    threshold = "TRACE",
    appenders = list(
      # to console for traking
      .createConsoleAppenderForSandboxLogging(),
      # to file for showing in app
      ParallelLogger::createFileAppender(
        fileName = file.path(folderWithLog, "log.txt"),
        layout = ParallelLogger::layoutSimple
      )
    )
  )
  ParallelLogger::clearLoggers()
  #addDefaultFileLogger(file.path(folderWithLog, "log2.txt"))
  ParallelLogger::registerLogger(logger)

  shiny::addResourcePath("logs", folderWithLog)

  return(logger)
}





