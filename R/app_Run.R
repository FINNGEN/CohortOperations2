#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(pathToCohortOperationsConfigYalm, pathToDatabasesConfigYalm, ...) {

  # set up configuration
  checkmate::assertFileExists(pathToCohortOperationsConfigYalm, extension = "yml")
  cohortOperationsConfig <- yaml::read_yaml(pathToCohortOperationsConfigYalm)
  checkmate::assertList(cohortOperationsConfig, names = "named")

  checkmate::assertFileExists(pathToDatabasesConfigYalm, extension = "yml")
  databasesConfig <- yaml::read_yaml(pathToDatabasesConfigYalm)
  checkmate::assertList(databasesConfig, names = "named")


  # set shiny to accept large files
  options(shiny.maxRequestSize = 1000000000)

  # deactivate https request to work with Atlas in https
  httr::set_config(httr::config(ssl_verifypeer = FALSE))

  # set up futuress
  future::plan(future::multisession, workers = 2)

  # set up loger
  fcr_setUpLogger()


  # start app
  app  <- shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    ...
  )

  # setup shiny options
  app$appOptions$cohortOperationsConfig  <- cohortOperationsConfig
  app$appOptions$databasesConfig  <- databasesConfig
  app$appOptions$logger  <- logger
  app$appOptions$cores  <- parallel::detectCores()-1
  app$appOptions$chunksSizeNOutcomes  <- 1000

  app$appOptions$pathToNews  <- here::here("NEWS.md")
  app$appOptions$gitInfo  <- paste("Branch: ", gert::git_info()$shorthand, "Commit: ", gert::git_info()$commit)

  ParallelLogger::logInfo("[Start] Start logging on ", app$appOptions$gitInfo)

  return(app)
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
