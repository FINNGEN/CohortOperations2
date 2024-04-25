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

  # set up future
  future::plan(future::multisession, workers = 2)

  # set up loger
  folderWithLog <- file.path(tempdir(), "logs")
  dir.create(folderWithLog, showWarnings = FALSE)
  logger <- ParallelLogger::createLogger(
    appenders = list(
      # to console for traking
      ParallelLogger::createConsoleAppender(
        layout = .layoutParallelWithHeader
      ),
      # to file for showing in app
      ParallelLogger::createFileAppender(
        fileName = file.path(folderWithLog, "log.txt"),
        layout = ParallelLogger::layoutSimple
      )
    )
  )
  ParallelLogger::clearLoggers()
  ParallelLogger::registerLogger(logger)

  shiny::addResourcePath("logs", folderWithLog)


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

  app$appOptions$pathToNews  <- here::here("NEWS.md")
  app$appOptions$gitInfo  <- paste("Branch: ", gert::git_info()$shorthand, "Commit: ", gert::git_info()$commit)

  ParallelLogger::logInfo("[Start] Start logging on ", app$appOptions$gitInfo)

  return(app)
}


.layoutParallelWithHeader  <- function(level, message){
  message <- paste0("[CO2] ", message)
  ParallelLogger::layoutParallel(level, message)
}


