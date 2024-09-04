


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



.createConsoleAppenderForSandboxLogging <- function(layout = ParallelLogger::layoutParallel) {
  appendFunction <- function(this, level, message, echoToConsole) {
    # Avoid note in check:
    missing(this)
    message <- paste0("[sandbox-co2-log] ", message)
    writeLines(message, con = stdout())

  }
  appender <- list(appendFunction = appendFunction, layout = layout)
  class(appender) <- "Appender"
  return(appender)
}


fct_stringToFuction <- function(packageFunctionString) {

  packageFunctionString |> checkmate::assertCharacter(pattern = "^\\w+::\\w+$")

  packageName   <- packageFunctionString |> stringr::str_split("::") |> purrr::pluck(1,1)
  functionName  <- packageFunctionString |> stringr::str_split("::") |> purrr::pluck(1,2)

 resultFunction <- rlang::ns_env(packageName)  |> rlang::env_get(functionName)

 return(resultFunction)
}





















