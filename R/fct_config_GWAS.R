
#' Configure GWAS API Connection
#'
#' This function configures and establishes a connection to the FinnGen internal GWAS API
#' using the `FinnGenUtilsR` package. It retrieves the sandbox token from the environment
#' and uses it to authenticate the connection to the sandbox API.
#'
#' @return A connection object to the FinnGen sandbox API.
#'
#' @importFrom FinnGenUtilsR createSandboxAPIConnection
#' @export
configGWAS <- function() {

  url <- "https://internal-api.app.finngen.fi/internal-api/"

  .sandbox_token <- Sys.getenv('SANDBOX_TOKEN')

  connection_sandboxAPI <- FinnGenUtilsR::createSandboxAPIConnection(url, .sandbox_token)

  return(connection_sandboxAPI)
}
