#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom shiny getShinyOption reactiveValues
#' @noRd
app_server <- function(input, output, session) {
  # set up loger
  fcr_setUpLogger()
  # log start
  ParallelLogger::logInfo("[Start] Start logging")

  # get settings loaded from file
  databasesConfig <- shiny::getShinyOption("databasesConfig")
  analysisModulesConfig <- shiny::getShinyOption("analysisModulesConfig")

  # This is a reactive value with cohortTableHandler and a counter to indicate a change with in the object
  # Because the cohortTableHandler is a pointe to an object, it changes only when the the selected database changes, but changes with in the object are not reflected
  # To comunicate changes with in the object, we use a counter: hasChangeCounter
  # Modules that modify information with in the object should increment this counter
  r_databaseConnection <- shiny::reactiveValues(
    cohortTableHandler = NULL,
    atlasConfig = NULL,
    hasChangeCounter = 0
  )


  mod_selectDatabases_server("selectDatabases", databasesConfig, r_databaseConnection)

  mod_cohortWorkbench_server("cohortWorkbench_importCohorts", r_databaseConnection)
  mod_importCohortsFromFile_server("importCohortsFromFile", r_databaseConnection)
  mod_importCohortsFromAtlas_server("importCohortsFromAtlas", r_databaseConnection)
  mod_importCohortsFromCohortsTable_server("importCohortsFromEndpoints", r_databaseConnection,
    filterCohortsRegex = "^(?!.*\\[CohortLibrary\\]).*$", filterCohortsName = "Endpoint"
  )
  mod_importCohortsFromCohortsTable_server("importCohortsFromLibrary", r_databaseConnection,
    filterCohortsRegex = ".*\\[CohortLibrary\\]", filterCohortsRegexRemove = "\\[CohortLibrary\\]", filterCohortsName = "Cohort Library"
  )

  mod_cohortWorkbench_server("cohortWorkbench_matchCohorts", r_databaseConnection)
  mod_matchCohorts_server("matchCohorts", r_databaseConnection)

  mod_cohortWorkbench_server("cohortWorkbench_operateCohorts", r_databaseConnection)
  mod_operateCohorts_server("operateCohorts", r_databaseConnection)

  mod_cohortWorkbench_server("cohortWorkbench_exportsCohorts", r_databaseConnection)
  mod_exportsCohorts_server("exportsCohorts", r_databaseConnection)

  # Add the new module server
  mod_viewResults_server("viewResults")

  # Dynamic analysis modules server
  lapply(names(analysisModulesConfig), function(analysisKey) {
    analysis <- analysisModulesConfig[[analysisKey]]
    mod_cohortWorkbench_server(paste0("cohortWorkbench_", analysisKey), r_databaseConnection)

    mod_analysisWrap_server(
      id = paste0(analysisKey, "_analysis"),
      r_databaseConnection = r_databaseConnection,
      mod_analysisSettings_server = analysis$mod_analysisSettings_server |> fct_stringToFuction(),
      fct_executeAnalysis = analysis$fct_executeAnalysis |> fct_stringToFuction(),
      analysisName = analysis$analysisName,
      url_visualiseResults = analysis$url_visualiseResults
    )
  })

  mod_appVersion_server("appVersion")
}
