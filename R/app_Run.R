#' Run Shiny Application
#'
#' This function initializes and runs a Shiny application with the specified configurations.
#'
#' @param databasesConfig A list containing the database configurations.
#' @param analysisModulesConfig A list containing the analysis modules configurations.
#' @param ... Additional arguments passed to `shiny::shinyApp`.
#'
#' @importFrom checkmate assertList
#' @importFrom httr set_config config
#' @importFrom shiny shinyApp
#' @importFrom ParallelLogger logInfo
#' 
#' @return A Shiny application object.
#' @export
run_app <- function(databasesConfig, analysisModulesConfig, ...) {

  # Check configuration files
  # TODO: check if the config files are correct
  databasesConfig  |> checkmate::assertList()
  analysisModulesConfig |> checkmate::assertList()

  #
  # GLOBAL SETTING
  #

  # set shiny to accept large files
  options(shiny.maxRequestSize = 10*1000*1024^2) # 10GB

  # deactivate https request to work with Atlas in https
  httr::set_config(httr::config(ssl_verifypeer = FALSE))

  # set up loger
  fcr_setUpLogger()

  #
  # Create app
  #

  app  <- shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    ...
  )

  # setup shiny options
  app$appOptions$databasesConfig  <- databasesConfig
  app$appOptions$analysisModulesConfig  <- analysisModulesConfig

  #
  # Launch app
  #

  # log start
  ParallelLogger::logInfo("[Start] Start logging on ", app$appOptions$gitInfo)

  return(app)
}
