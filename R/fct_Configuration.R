


#' Check Databases Configuration
#'
#' Checks the provided databases configuration and returns TRUE if the configuration is valid.
#' If the configuration is invalid, it returns the error messages.
#'
#' @param databasesConfig A list representing the databases configuration.
#'
#' @return TRUE if the configuration is valid, otherwise a list of error messages.
#'
#' @export
fct_checkdatabasesConfig  <- function(databasesConfig) {
  collection <- .fct_assertdatabasesConfig(databasesConfig)
  if (collection$isEmpty()) {
    return(TRUE)
  } else {
    return(collection$getMessages())
  }
}
#' Assert Databases Configuration
#'
#' Asserts the provided databases configuration and reports any assertion errors.
#'
#' @param databasesConfig A list representing the databases configuration.
#'
#' @return None. Throws an error if the configuration is invalid.
#'
#' @export
fct_assertdatabasesConfig  <- function(databasesConfig) {
  collection <- .fct_assertdatabasesConfig(databasesConfig)
  if (!collection$isEmpty()) {
    checkmate::reportAssertions(collection)
  }
}

#' Internal Assert Databases Configuration
#'
#' Internal function to assert the provided databases configuration.
#'
#' @param databasesConfig A list representing the databases configuration.
#'
#' @return An AssertCollection object containing any assertion errors.
#'
.fct_assertdatabasesConfig <- function(databasesConfig) {
  collection = checkmate::makeAssertCollection()
  databasesConfig |> checkmate::assertList()
  # TODO check structure
  return(collection)
}

#' Set Up Logger
#'
#' Sets up a logger with console and file appenders for logging.
#'
#' @return A logger object.
#'
#' @export
fcr_setUpLogger  <- function(){
  folderWithLog <- file.path(tempdir(), "logs")
  dir.create(folderWithLog, showWarnings = FALSE)
  logger <- ParallelLogger::createLogger(
    threshold = "TRACE",
    appenders = list(
      # to console for tracking
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


#' Create Console Appender for Sandbox Logging
#'
#' Creates a console appender for sandbox logging with a specified layout.
#'
#' @param layout A layout function for the logger. Defaults to ParallelLogger::layoutParallel.
#'
#' @return An appender object for logging.
#'
.createConsoleAppenderForSandboxLogging <- function(layout = ParallelLogger::layoutParallel) {
  appendFunction <- function(this, level, message, echoToConsole) {
    # Avoid note in check:
    missing(this)
    message <- paste0("[sandbox-co2-log] ", message)
    writeLines(message, con = stderr())
  }
  appender <- list(appendFunction = appendFunction, layout = layout)
  class(appender) <- "Appender"
  return(appender)
}

#' Convert String to Function
#'
#' Converts a string in the format "package::function" to the actual function.
#'
#' @param packageFunctionString A string representing the package and function in the format "package::function".
#'
#' @return The function object specified by the string.
#'
#' @export
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


















