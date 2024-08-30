#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # get settings loaded from file
  databasesConfig <- shiny::getShinyOption("databasesConfig")

  # This is a reactive value with cohortTableHandler and a counter to indicate a change with in the object
  # Because the cohortTableHandler is a pointe to an object, it changes only when the the selected database changes, but changes with in the object are not reflected
  # To comunicate changes with in the object, we use a counter: hasChangeCounter
  # Modules that modify information with in the object should increment this counter
  r_connectionHandler <- shiny::reactiveValues(
    cohortTableHandler = NULL,
    hasChangeCounter = 0
  )


  mod_selectDatabases_server("selectDatabases", databasesConfig, r_connectionHandler)

  mod_cohortWorkbench_server("cohortWorkbench_importCohorts", r_connectionHandlers)
  mod_importCohortsFromFile_server("importCohortsFromFile", r_connectionHandlers)
  # mod_importCohortsFromAtlas_server("importCohortsFromAtlas", r_connectionHandlers, r_workbench)
  # mod_importCohortsFromCohortsTable_server("importCohortsFromEndpoints", r_connectionHandlers, r_workbench)
  # mod_importCohortsFromAtlas_server("importCohortsFromLibrary", r_connectionHandlers, r_workbench, filterCohortsRegex = ".*\\[CohortLibrary]")
  #
  # mod_cohortWorkbench_server("cohortWorkbench_matchCohorts", r_connectionHandlers, r_workbench)
  # mod_matchCohorts_server("matchCohorts", r_connectionHandlers, r_workbench)
  #
  # mod_cohortWorkbench_server("cohortWorkbench_operateCohorts", r_connectionHandlers, r_workbench)
  # mod_operateCohorts_server("operateCohorts", r_connectionHandlers, r_workbench)
  #
  # mod_cohortWorkbench_server("cohortWorkbench_exportsCohorts", r_connectionHandlers, r_workbench)
  # mod_exportsCohorts_server("exportsCohorts", r_connectionHandlers, r_workbench)
  #
  # mod_cohortWorkbench_server("cohortWorkbench_cohortDiagnostics", r_connectionHandlers, r_workbench)
  # mod_cohortDiagnostics_server("cohortDiagnostics", r_connectionHandlers, r_workbench)
  #
  # mod_cohortWorkbench_server("cohortWorkbench_cohortOverlaps", r_connectionHandlers, r_workbench)
  # mod_cohortOverlaps_server("cohortOverlaps", r_connectionHandlers, r_workbench)
  #
  # mod_cohortWorkbench_server("cohortWorkbench_cohortsIncidence", r_connectionHandlers, r_workbench)
  # mod_cohortsIncidence_server("cohortsIncidence", r_connectionHandlers, r_workbench)
  #
  # mod_cohortWorkbench_server("cohortWorkbench_timeCodeWAS", r_connectionHandlers, r_workbench)
  # mod_timeCodeWAS_server("timeCodeWAS", r_connectionHandlers, r_workbench)
  #
  # mod_cohortWorkbench_server("cohortWorkbench_codeWAS", r_connectionHandlers, r_workbench)
  # mod_codeWAS_server("codeWAS", r_connectionHandlers, r_workbench)
  #
  # mod_cohortWorkbench_server("cohortWorkbench_runGWAS", r_connectionHandlers, r_workbench)
  # mod_runGWAS_server("runGWAS", r_connectionHandlers, r_workbench)
  #
  #
  # output$about <- shiny::renderUI({
  #   # load news from shinyoption pathtomd
  #
  #   shiny::div(
  #     shiny::markdown(readLines(shiny::getShinyOption("pathToNews"))),
  #     shiny::br(),
  #     shiny::p(shiny::getShinyOption("gitInfo"))
  #   )
  #
  # })



}


