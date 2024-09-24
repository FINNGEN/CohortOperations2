
#' Title
#'
#' @return
#' @export
#'
#' @examples
configGWAS <- function() {

  url <- "https://internal-api.app.finngen.fi/internal-api/"

  path <- file.path(Sys.getenv("PATH_TO_SANDBOX_TOKEN"), '.sandbox_token')

  .sandbox_token <- readLines(path, warn=F)

  connection_sandboxAPI <- FinnGenUtilsR::createSandboxAPIConnection(url, .sandbox_token)

  return(connection_sandboxAPI)
}
