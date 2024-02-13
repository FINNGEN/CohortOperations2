


fct_getDatabaseIdNamesListFromConfigurationList <- function(configurationList) {

  fct_assertConfigurationList(configurationList)

  databaseIdNamesList <- list()
  for(databaseId in names(configurationList)){
    databaseIdNamesList[[configurationList[[databaseId]]$cohortTableHandler$database$databaseName]] <- databaseId
  }

  return(databaseIdNamesList)

}




fct_configurationListToDatabasesHandlers <- function(
    configurationList,
    loadConnectionChecksLevel = "basicChecks") {

  fct_assertConfigurationList(configurationList)

  databasesHandlers <- list()
  for(databaseId in names(configurationList)){

    cohortTableHandlerConfig <- configurationList[[databaseId]]$cohortTableHandler

    cohortTableHandler <- HadesExtras::createCohortTableHandlerFromList(cohortTableHandlerConfig, loadConnectionChecksLevel)

    databasesHandlers[[databaseId]] <- list(cohortTableHandler = cohortTableHandler)
  }

  return(databasesHandlers)

}



fct_checkConfigurationList  <- function(configurationList) {
  collection <- .fct_assertConfigurationList(configurationList)
  if (collection$isEmpty()) {
    return(TRUE)
  } else {
    return(collection$getMessages())
  }
}

fct_assertConfigurationList  <- function(configurationList) {
  collection <- .fct_assertConfigurationList(configurationList)
  if (!collection$isEmpty()) {
    checkmate::reportAssertions(collection)
  }
}

.fct_assertConfigurationList <- function(configurationList) {

  collection = checkmate::makeAssertCollection()

  configurationList |> checkmate::assertList()
  # TODO check structure

  return(collection)

}







