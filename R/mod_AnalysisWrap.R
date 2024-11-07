#' Analysis Wrap UI
#'
#' @param id A unique identifier for the module.
#' @param mod_analysisSettings_ui A UI function for the analysis settings module.
#'
#' @return A UI definition for the analysis wrap module.
#'
#' @importFrom shiny NS actionButton verbatimTextOutput downloadButton tags
#' @importFrom shinyjs useShinyjs
#' @importFrom htmltools tagList hr
#'
#' @export
mod_analysisWrap_ui <- function(id, mod_analysisSettings_ui) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyjs::useShinyjs(),
    htmltools::hr(),
    #
    mod_analysisSettings_ui(ns("analysisWrap")),
    #
    shiny::actionButton(ns("runAnalysis_actionButton"), "Run Study"),
    #
    htmltools::hr(),
    shiny::tags$h4("Results"),
    shiny::verbatimTextOutput(ns("results_text")),
    shiny::tags$br(),
    shiny::downloadButton(ns("download_actionButton"), "Download to Sandbox"),
    #shiny::downloadButton(ns("download_actionButton2"), "Download out of Sandbox"),
    shiny::actionButton(ns("view_actionButton"), "Open Viewer"),
  )
}

#' Analysis Wrap Server
#'
#' @param id A unique identifier for the module.
#' @param r_databaseConnection A reactive database connection object.
#' @param mod_analysisSettings_server A server function for the analysis settings module.
#' @param fct_executeAnalysis A function to execute the analysis.
#' @param analysisName The name of the analysis.
#' @param url_visualiseResults The URL to visualize the results.
#'
#' @return A server function for the analysis wrap module.
#'
#' @importFrom shiny moduleServer reactiveValues observe observeEvent req renderText downloadHandler
#' @importFrom shinyjs toggleState runjs
#' @importFrom ParallelLogger logInfo
#'
#' @export
mod_analysisWrap_server <- function(id, r_databaseConnection, mod_analysisSettings_server, fct_executeAnalysis, analysisName, url_visualiseResults) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      analysisSettings = NULL,
      analysisResults = NULL
    )

    #
    # Call analysisSettings module
    #
    rf_analysisSettings <- mod_analysisSettings_server("analysisWrap", r_databaseConnection)
    shiny::observe({
      r$analysisSettings <- rf_analysisSettings()
    })

    #
    # activate Run study if settings are valid
    #
    shiny::observe({
      ParallelLogger::logInfo("[Analysis: ", analysisName,"] Analysis Settings changed: ", r$analysisSettings)
      condition <- shiny::isTruthy(r$analysisSettings)
      shinyjs::toggleState("runAnalysis_actionButton", condition = condition )
    })

    #
    # click to run
    #
    shiny::observeEvent(input$runAnalysis_actionButton, {
      shiny::req(r$analysisSettings)

      exportFolder <- file.path(tempdir(), gsub("[^0-9]", "",format(Sys.time(), "d%H%M%S%OS3")))
      dir.create(exportFolder)
      ParallelLogger::logInfo("[Analysis: ", analysisName,"] Create tmp folder for analysis results: ", exportFolder)
      cohortTableHandler <- r_databaseConnection$cohortTableHandler
      analysisSettings <- r$analysisSettings

      ParallelLogger::logInfo("[Analysis: ", analysisName,"] Start analysis")
      fct_sweetAlertSpinner("Running Analysis")
      startTime <- Sys.time()

      pathToResultsDatabase <- NULL
      analysisError <- NULL
      pathToResultsDatabase <- tryCatch({
        fct_executeAnalysis(
          exportFolder = exportFolder,
          cohortTableHandler = cohortTableHandler,
          analysisSettings = analysisSettings
        )
      }, error = function(e) {
        analysisError <<- e$message
        pathToResultsDatabase <<- NULL
      })

      ParallelLogger::logInfo("[Analysis: ", analysisName,"] End analysis. Result: ", pathToResultsDatabase)
      fct_removeSweetAlertSpinner()
      analysisDuration <- Sys.time() - startTime

      analysisResults <- list(
        pathToResultsDatabase = pathToResultsDatabase,
        analysisError = analysisError,
        analysisDuration = analysisDuration
      )

      r$analysisResults <- analysisResults

    })

    #
    # display results
    #
    output$results_text <- shiny::renderText({
      if(!shiny::isTruthy(r$analysisResults)){
        return(NULL)
      }

      analysisDurationMins <- r$analysisResults$analysisDuration |> as.numeric(units = "mins")
      analysisDurationText <- paste0(
        floor(analysisDurationMins), " minutes and ",
        round((analysisDurationMins-floor(analysisDurationMins))*60), " seconds"
      )

      # if successful
      if(is.null(r$analysisResults$analysisError)){
        resultMessage <- paste0("\u2705 Success\n",
                                "\U0001F550 Running time: ", analysisDurationText, "\n"
        )
      }else{
        resultMessage <- paste0("\u274C Error\n",
                                "\U0001F4C4 Message: ",  r$analysisResults$analysisError, "\n")
      }

      ParallelLogger::logInfo("[Analysis: ", analysisName,"] Results message: ", resultMessage)

      resultMessage
    })

    #
    # activate Run study if settings are valid
    #
    shiny::observe({
      condition <- shiny::isTruthy(r$analysisResults) &&
        is.null(r$analysisResults$analysisError)
      shinyjs::toggleState("download_actionButton", condition = condition )
      shinyjs::toggleState("view_actionButton", condition = condition )
    })


    #
    # download results
    #
    output$download_actionButton <- shiny::downloadHandler(
      filename = function(){paste0(analysisName, "_analysisResults.sqlite")},
      content = function(fname){
        condition <- shiny::isTruthy(r$analysisResults) &&
          is.null(r$analysisResults$analysisError)

        if(condition){
          file.copy(r$analysisResults$pathToResultsDatabase, fname)
        }

        ParallelLogger::logInfo("[Analysis: ", analysisName,"] Download results")

        return(fname)
      }
    )

    #
    # Launch viewer
    #
    shiny::observeEvent(input$view_actionButton, {
      shiny::req(r$analysisResults)

      ParallelLogger::logInfo("[Analysis: ", analysisName,"] Launch viewer")

      if(is.null(r$analysisResults$pathToResultsDatabase)){
        url <- paste0(url_visualiseResults)
      }else{
        url <- paste0(url_visualiseResults, r$analysisResults$pathToResultsDatabase)
      }

      shinyjs::runjs(paste0("window.open('", url, "')"))
    })

  })
}





















