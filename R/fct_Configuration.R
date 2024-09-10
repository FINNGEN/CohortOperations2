


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




#' Read and Parse YAML File with Placeholder Replacement
#'
#' Reads a YAML file from a given path, replaces specified placeholders with provided values,
#' and returns the parsed content. If any provided placeholders are not found in the YAML file,
#' the function throws an error.
#'
#' @param pathToYalmFile A string representing the file path to the YAML file.
#' @param ... Named arguments to replace placeholders in the format `<name>` within the YAML file.
#'
#' @return A parsed list representing the contents of the modified YAML file.
#'
#' @importFrom yaml yaml.load as.yaml
#'
#' @export
readAndParseYalm <- function(pathToYalmFile, ...) {
  # read the yaml file
  yalmString <- readLines(pathToYalmFile) |> paste(collapse = "\n")
  # get the names of the parameters
  args <- list(...)
  argsNames <- names(args)

  # check for missing placeholders
  missingParams <- argsNames[!sapply(argsNames, function(name) any(grepl(paste0("<", name, ">"), yalmString)))]

  # if any placeholders are not found, throw an error
  if (length(missingParams) > 0) {
    stop(paste("Error: The following placeholders were not found in the YAML file:", paste(missingParams, collapse = ", ")))
  }

  # replace the values in the yaml file
  for (name in argsNames) {
    yalmString <- gsub(paste0("<", name, ">"), args[[name]], yalmString)
  }

  # parse the yaml file
  yalmFile <- yaml::yaml.load(yalmString)
  return(yalmFile)
}


















