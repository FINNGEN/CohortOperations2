#' View Results UI Module
#'
#' @description A shiny Module to view results from a file.
#'
#' @param id Module ID
#'
#' @importFrom shiny NS tagList
#'
#' @return A UI definition for the module.
mod_viewResults_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyjs::useShinyjs(),
    shinyWidgets::useSweetAlert(),
    shiny::fileInput(ns("importModal_button"), "Upload Results File"),
    shiny::uiOutput(ns("alert_error"))
  )
}

#' View Results Server Module
#'
#' @description A shiny Module to handle server-side operations for viewing results from a file.
#'
#' @param id Module ID
#'
#' @importFrom shiny moduleServer reactiveValues observeEvent
#' @importFrom reactable reactableOutput renderReactable
#' @importFrom duckdb dbConnect 
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows filter pull arrange
#' @importFrom DBI dbListTables dbDisconnect
#' 
#' @return A server function for the module.
mod_viewResults_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    analysisModulesConfig <- shiny::getShinyOption("analysisModulesConfig")

    output$alert_error <- shiny::renderUI({
      shinyWidgets::alert(
        "Upload a previously saved results file to view the results.",
        id = ns("alert_error"),
        status = "info"
      )
    })

    shiny::observeEvent(input$importModal_button, {
      # Get the file extension
      fileExt <- tools::file_ext(input$importModal_button$name)

      # Handle DuckDB file
      if (fileExt != "duckdb") {
        output$alert_error <- shiny::renderUI({
          shinyWidgets::alert(
            "Please upload a DuckDB file.",
            id = ns("alert_error"), 
            status = "danger"
          )
        })
        return(NULL)
      }

      analysisResults <- duckdb::dbConnect(duckdb::duckdb(), input$importModal_button$datapath)
      if (!("analysisInfo" %in% DBI::dbListTables(analysisResults))) {
        output$alert_error <- shiny::renderUI({
          shinyWidgets::alert(
            "The file does not contain required analysisInfo table.",
            id = ns("alert_error"), 
            status = "danger"
          )
        })
        return(NULL)
      }
      analysisInfo <- analysisResults |> dplyr::tbl("analysisInfo") |> dplyr::collect()
      DBI::dbDisconnect(analysisResults)
      if (!("analysisType" %in% names(analysisInfo))) {
        output$alert_error <- shiny::renderUI({
          shinyWidgets::alert(
            "The analysisInfo table is missing the analysisType column.",
            id = ns("alert_error"), 
            status = "danger"
          )
        })
        return(NULL)
      }

      analysisType <- analysisInfo |> dplyr::pull(analysisType)
      tibbleOfAnalysisTypesAndURLs <- tibble::tibble()
      for (analysisModule in analysisModulesConfig) {
        if (exists("analysisType", analysisModule)) {
          tibbleOfAnalysisTypesAndURLs <- tibbleOfAnalysisTypesAndURLs |>
            dplyr::bind_rows(tibble::tibble(analysisType = analysisModule$analysisType, url_visualiseResults = analysisModule$url_visualiseResults))
        }
      }

      if (!(analysisType[1] %in% tibbleOfAnalysisTypesAndURLs$analysisType)) {
        output$alert_error <- shiny::renderUI({
          shinyWidgets::alert(
            "The analysis type is not supported.",
            id = ns("alert_error"), 
            status = "danger"
          )
        })
        return(NULL)
      }
      
      url_visualiseResults <- tibbleOfAnalysisTypesAndURLs |>
        dplyr::filter(analysisType == analysisType[1]) |>
        dplyr::pull(url_visualiseResults)

      url <- paste0(url_visualiseResults, input$importModal_button$datapath)

      output$alert_error <- shiny::renderUI({ 
        shinyWidgets::alert(
          "Imported results file successfully. Viewer has been opened in a new tab.",
          id = ns("alert_error"), 
          status = "success"
          )
        })

      shinyjs::runjs(paste0("window.open('", url, "')"))

    })
  })
}
