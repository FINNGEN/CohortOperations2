#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # get settings loaded from file
  databasesConfig <- shiny::getShinyOption("databasesConfig")

  # list of connection handlers that are passed to modules,
  # not all modules used all, they modules check the list has at least the ones they need
  # they are produced only by mod_select_configuration and consumed by the modules
  r_connectionHandlers <- shiny::reactiveValues(
    databasesHandlers = NULL
  )

  # produced by modules related to cohort editing
  # consumed by cohort table viewer
  r_workbench <- shiny::reactiveValues(
    cohortsSummaryDatabases = HadesExtras::createEmptyCohortsSummary()
  )

  shiny::observe({
    sweetAlert_spinner("Starting application")
  })

  mod_selectDatabases_server("selectDatabases", databasesConfig, r_connectionHandlers)

  mod_cohortWorkbench_server("cohortWorkbench_importCohorts", r_connectionHandlers, r_workbench)
  mod_importCohortsFromFile_server("importCohortsFromFile", r_connectionHandlers, r_workbench)
  mod_importCohortsFromAtlas_server("importCohortsFromAtlas", r_connectionHandlers, r_workbench)
  mod_importCohortsFromCohortsTable_server("importCohortsFromEndpoints", r_connectionHandlers, r_workbench)
  mod_importCohortsFromAtlas_server("importCohortsFromLibrary", r_connectionHandlers, r_workbench, filterCohortsRegex = ".*\\[CohortLibrary]")

  mod_cohortWorkbench_server("cohortWorkbench_matchCohorts", r_connectionHandlers, r_workbench)
  mod_matchCohorts_server("matchCohorts", r_connectionHandlers, r_workbench)

  mod_cohortWorkbench_server("cohortWorkbench_operateCohorts", r_connectionHandlers, r_workbench)
  mod_operateCohorts_server("operateCohorts", r_connectionHandlers, r_workbench)

  mod_cohortWorkbench_server("cohortWorkbench_cohortDiagnostics", r_connectionHandlers, r_workbench)
  mod_cohortDiagnostics_server("cohortDiagnostics", r_connectionHandlers, r_workbench)

  mod_cohortWorkbench_server("cohortWorkbench_cohortsIncidence", r_connectionHandlers, r_workbench)
  mod_cohortsIncidence_server("cohortsIncidence", r_connectionHandlers)


  mod_cohortWorkbench_server("cohortWorkbench_timeCodeWAS", r_connectionHandlers, r_workbench)
  mod_timeCodeWAS_server("timeCodeWAS", r_connectionHandlers, r_workbench)


  output$about <- shiny::renderUI({
    # load news from shinyoption pathtomd

    shiny::div(
      shiny::markdown(readLines(shiny::getShinyOption("pathToNews"))),
      shiny::br(),
      shiny::p(shiny::getShinyOption("gitInfo"))
    )

  })



}


