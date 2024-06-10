


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
}





