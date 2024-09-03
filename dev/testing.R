devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

databasesHandlers <- helper_createNewDatabaseHandlers(withEunomiaCohorts = TRUE)

CohortDiagnostics:: executeDiagnostics(
  cohortDefinitionSet = databasesHandlers$dev_eunomia$cohortTableHandler$cohortDefinitionSet,
  exportFolder = file.path(tempdir(), "cohortDiagnostics"),
  databaseId = databasesHandlers$dev_eunomia$cohortTableHandler$databaseName,
  cohortDatabaseSchema = databasesHandlers$dev_eunomia$cohortTableHandler$cohortDatabaseSchema,
  databaseName = databasesHandlers$dev_eunomia$cohortTableHandler$databaseName,
  databaseDescription = databasesHandlers$dev_eunomia$cohortTableHandler$databaseName,
  connection = databasesHandlers$dev_eunomia$cohortTableHandler$connectionHandler$getConnection(),
  cdmDatabaseSchema = databasesHandlers$dev_eunomia$cohortTableHandler$cdmDatabaseSchema,
  cohortTable = databasesHandlers$dev_eunomia$cohortTableHandler$cohortTableNames$cohortTable,
  vocabularyDatabaseSchema = databasesHandlers$dev_eunomia$cohortTableHandler$vocabularyDatabaseSchema,
  cohortIds = 1:3,
  # runInclusionStatistics = FALSE,
  # runIncludedSourceConcepts = FALSE,
  # runOrphanConcepts = FALSE,
  # runTimeSeries = FALSE,
  # runVisitContext = FALSE,
  # runBreakdownIndexEvents = FALSE,
  # runIncidenceRate = TRUE,
  # runCohortRelationship = FALSE,
  # runTemporalCohortCharacterization = FALSE,
  minCellCount = 1,
  incremental = FALSE)

CohortDiagnostics::createMergedResultsFile(file.path(tempdir(), "cohortDiagnostics"), sqliteDbPath = file.path(tempdir(),"MyCohortDiagnosticsResulst.sqlite"),overwrite = TRUE)

connectionHandler <- ResultModelManager::ConnectionHandler$new(
  connectionDetails = DatabaseConnector::createConnectionDetails(dbms = "sqlite", server = file.path(tempdir(),"MyCohortDiagnosticsResulst.sqlite"))
)

cdData  <- OhdsiShinyModules::createCdDatabaseDataSource(
  connectionHandler,
  resultDatabaseSettings = list(schema = "main")
)


app <- shiny::shinyApp(
  shiny::fluidPage(
    OhdsiShinyModules::incidenceRatesView("incidenceRates")
  ),
  function(input,output,session){
      OhdsiShinyModules:::incidenceRatesModule(
        id="incidenceRates",
        dataSource = cdData,
        selectedCohorts = shiny::reactive({cdData$cohortTable$cohortName}),
        selectedDatabaseIds = shiny::reactive({ "eunomia1"}),
        cohortIds = shiny::reactive({1:3}),
        databaseTable = cdData$dbTable,
        cohortTable = cdData$cohortTable
      )
  },
  options = list(launch.browser=TRUE)
)


app
