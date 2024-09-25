
#' Title
#'
#' @return
#' @export
#'
#' @examples
configGWAS <- function() {

  url <- "https://internal-api.app.finngen.fi/internal-api/"

  .sandbox_token <- Sys.getenv('SANDBOX_TOKEN')

  connection_sandboxAPI <- FinnGenUtilsR::createSandboxAPIConnection(url, .sandbox_token)

  return(connection_sandboxAPI)
}
