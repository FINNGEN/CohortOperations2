


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













