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



  # set options
  options(shiny.maxRequestSize = 1000000000)

  # deactivate https request
  httr::set_config(httr::config(ssl_verifypeer = FALSE))

  # set up logger
  logger <- setup_ModalWithLog()

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

    return(app)
}
