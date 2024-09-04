#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(pathToDatabasesConfigYalm, pathToAnalysisModulesConfigYalm, ...) {

  # Check configuration files
  # TODO: check if the config files are correct
  checkmate::assertFileExists(pathToDatabasesConfigYalm, extension = "yml")
  databasesConfig <- yaml::read_yaml(pathToDatabasesConfigYalm)

  checkmate::assertFileExists(pathToAnalysisModulesConfigYalm, extension = "yml")
  analysisModulesConfig <- yaml::read_yaml(pathToAnalysisModulesConfigYalm)

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

  app$appOptions$pathToNews  <- here::here("NEWS.md")
  app$appOptions$gitInfo  <- paste("Branch: ", gert::git_info()$shorthand, "Commit: ", gert::git_info()$commit)

  #
  # Launch app
  #

  # log start
  ParallelLogger::logInfo("[Start] Start logging on ", app$appOptions$gitInfo)

  return(app)
}

