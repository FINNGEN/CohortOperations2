
helper_createNewCohortTableHandler <- function(addCohorts = NULL){

  addCohorts |> checkmate::assertCharacter(len = 1, null.ok = TRUE)
  addCohorts |> checkmate::assertSubset(c(
    "EunomiaDefaultCohorts", "HadesExtrasFractureCohorts","HadesExtrasAsthmaCohorts",
    "HadesExtrasFractureCohortsMatched","HadesExtrasAsthmaCohortsMatched"
  ), empty.ok = TRUE)

  cohortTableHandlerConfig <- cohortTableHandlerConfig # set by setup.R
  loadConnectionChecksLevel = "basicChecks"

  # TEMP
  # name with current time with milisecond to avoid conflicts
  cohortTableHandlerConfig$cohortTable$cohortTableName <- paste0(
    cohortTableHandlerConfig$cohortTable$cohortTableName, "_",
    gsub('\\.','',format(Sys.time(), "d%H%M%S%OS3"))
  )
  # END TEMP
  cohortTableHandler <- HadesExtras::createCohortTableHandlerFromList(cohortTableHandlerConfig, loadConnectionChecksLevel)

  if(!is.null(addCohorts) ){
    if(addCohorts == "EunomiaDefaultCohorts"){
      cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
        settingsFileName = "testdata/name/Cohorts.csv",
        jsonFolder = "testdata/name/cohorts",
        sqlFolder = "testdata/name/sql/sql_server",
        cohortFileNameFormat = "%s",
        cohortFileNameValue = c("cohortName"),
        packageName = "CohortGenerator",
        verbose = FALSE
      )
    }
    if(addCohorts == "HadesExtrasFractureCohorts"){
      cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
        settingsFileName = "testdata/fracture/Cohorts.csv",
        jsonFolder = "testdata/fracture/cohorts",
        sqlFolder = "testdata/fracture/sql/sql_server",
        cohortFileNameFormat = "%s",
        cohortFileNameValue = c("cohortId"),
        subsetJsonFolder = "testdata/fracture/cohort_subset_definitions/",
        packageName = "HadesExtras",
        verbose = T
      )
    }
    if(addCohorts == "HadesExtrasAsthmaCohorts"){
      cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
        settingsFileName = "testdata/asthma/Cohorts.csv",
        jsonFolder = "testdata/asthma/cohorts",
        sqlFolder = "testdata/asthma/sql/sql_server",
        cohortFileNameFormat = "%s",
        cohortFileNameValue = c("cohortId"),
        subsetJsonFolder = "testdata/asthma/cohort_subset_definitions/",
        packageName = "HadesExtras",
        verbose = FALSE
      )
    }
    if(addCohorts == "HadesExtrasFractureCohortsMatched"){
      cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
        settingsFileName = "testdata/fracture/Cohorts.csv",
        jsonFolder = "testdata/fracture/cohorts",
        sqlFolder = "testdata/fracture/sql/sql_server",
        cohortFileNameFormat = "%s",
        cohortFileNameValue = c("cohortId"),
        subsetJsonFolder = "testdata/fracture/cohort_subset_definitions/",
        packageName = "HadesExtras",
        verbose = T
      )

      # Match
      subsetDef <- CohortGenerator::createCohortSubsetDefinition(
        name = "",
        definitionId = 1,
        subsetOperators = list(
          HadesExtras::createMatchingSubset(
            matchToCohortId = 1,
            matchRatio = 10,
            matchSex = TRUE,
            matchBirthYear = TRUE,
            matchCohortStartDateWithInDuration = FALSE,
            newCohortStartDate = "asMatch",
            newCohortEndDate = "keep"
          )
        )
      )

      cohortDefinitionSet <- cohortDefinitionSet |>
        CohortGenerator::addCohortSubsetDefinition(subsetDef, targetCohortIds = 2)
    }
    if(addCohorts == "HadesExtrasAsthmaCohortsMatched"){
      # cohorts from eunomia
      cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
        settingsFileName = "testdata/asthma/Cohorts.csv",
        jsonFolder = "testdata/asthma/cohorts",
        sqlFolder = "testdata/asthma/sql/sql_server",
        cohortFileNameFormat = "%s",
        cohortFileNameValue = c("cohortId"),
        subsetJsonFolder = "testdata/asthma/cohort_subset_definitions/",
        packageName = "HadesExtras",
        verbose = FALSE
      )

      # Match to sex and bday, match ratio 10
      subsetDef <- CohortGenerator::createCohortSubsetDefinition(
        name = "",
        definitionId = 1,
        subsetOperators = list(
          HadesExtras::createMatchingSubset(
            matchToCohortId = 1,
            matchRatio = 10,
            matchSex = TRUE,
            matchBirthYear = TRUE,
            matchCohortStartDateWithInDuration = FALSE,
            newCohortStartDate = "asMatch",
            newCohortEndDate = "keep"
          )
        )
      )

      cohortDefinitionSet <- cohortDefinitionSet |>
        CohortGenerator::addCohortSubsetDefinition(subsetDef, targetCohortIds = 2)

    }
    cohortTableHandler$insertOrUpdateCohorts(cohortDefinitionSet)
  }

  return(cohortTableHandler)
}
#
#
# helper_createNewDatabaseHandlers <- function(withEunomiaCohorts = FALSE, withCohortTableCohorts = FALSE) {
#
#   databasesHandlers <- fct_databasesConfigToDatabasesHandlers(databasesConfig)
#
#   if(withEunomiaCohorts==TRUE){
#
#     cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
#       settingsFileName = "testdata/name/Cohorts.csv",
#       jsonFolder = "testdata/name/cohorts",
#       sqlFolder = "testdata/name/sql/sql_server",
#       cohortFileNameFormat = "%s",
#       cohortFileNameValue = c("cohortName"),
#       packageName = "CohortGenerator",
#       verbose = FALSE
#     )
#
#     for (databaseId in names(databasesHandlers)) {
#       databasesHandlers[[databaseId]]$cohortTableHandler$insertOrUpdateCohorts(cohortDefinitionSet)
#     }
#
#   }
#
#   if(withCohortTableCohorts==TRUE){
#
#     testCohortTable <-  tibble::tribble(
#       ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
#       1, 1, as.Date("2000-01-01"), as.Date("2000-12-01"),
#       1, 2, as.Date("2000-01-01"), as.Date("2000-12-01"),
#       1, 3, as.Date("2000-01-01"), as.Date("2000-12-01"),
#       1, 4, as.Date("2000-01-01"), as.Date("2000-12-01"),
#       1, 5, as.Date("2000-01-01"), as.Date("2000-12-01"),
#       1, 5, as.Date("2004-01-01"), as.Date("2004-12-01"),
#
#       2, 2, as.Date("2001-01-01"), as.Date("2002-12-01"),# non overplaping
#       2, 3, as.Date("2000-06-01"), as.Date("2000-09-01"),# inside
#       2, 4, as.Date("2000-06-01"), as.Date("2010-12-01"),# overlap
#       2, 5, as.Date("2004-06-01"), as.Date("2010-12-01"),# overlap with second
#       2, 6, as.Date("2000-01-01"), as.Date("2010-12-01")
#     )
#
#     testCohortDefinitionTable <- tibble::tribble(
#       ~cohort_definition_id, ~cohort_definition_name, ~cohort_definition_description, ~definition_type_concept_id, ~cohort_definition_syntax, ~subject_concept_id, ~cohort_initiation_date,
#       1, 'Diabetes Cohort', 'Cohort of patients diagnosed with diabetes', 1234, 'SELECT * FROM patients WHERE diagnosis = "Diabetes"', 5678, as.Date('2022-01-01'),
#       2, 'Hypertension Cohort', 'Cohort of patients diagnosed with hypertension', 1234, 'SELECT * FROM patients WHERE diagnosis = "Hypertension"', 5678, as.Date('2022-01-01'),
#       3, 'Obesity Cohort', 'Cohort of patients diagnosed with obesity', 1234, 'SELECT * FROM patients WHERE diagnosis = "Obesity"', 5678, as.Date('2022-01-01')
#     )
#
#     for (databaseId in names(databasesHandlers)) {
#
#       DatabaseConnector::insertTable(
#         connection = databasesHandlers[[databaseId]]$cohortTableHandler$connectionHandler$getConnection(),
#         table = "cohort",
#         data = testCohortTable
#       )
#
#       DatabaseConnector::insertTable(
#         connection = databasesHandlers[[databaseId]]$cohortTableHandler$connectionHandler$getConnection(),
#         table = "cohort_definition",
#         data = testCohortDefinitionTable
#       )
#     }
#
#   }
#
#
#   return(databasesHandlers)
#
# }


#
#
# helper_createTestCohortTableHandler <- function(configCohortTableHandler, withEunomiaCohorts = FALSE) {
#
#   connectionHandler <- HadesExtras::ResultModelManager_createConnectionHandler(
#     connectionDetailsSettings = configCohortTableHandler$connection$connectionDetailsSettings
#   )
#
#   cohortTableHandler <- HadesExtras::CohortTableHandler$new(
#     connectionHandler = connectionHandler,
#     cdmDatabaseSchema = configCohortTableHandler$cdm$cdmDatabaseSchema,
#     vocabularyDatabaseSchema = configCohortTableHandler$cdm$vocabularyDatabaseSchema,
#     cohortDatabaseSchema = configCohortTableHandler$cohortTable$cohortDatabaseSchema,
#     cohortTableName = configCohortTableHandler$cohortTable$cohortTableName
#   )
#
#   if(withEunomiaCohorts==TRUE){
#     eunomiaCohorts <- Eunomia::createCohorts(
#       connectionDetails = cohortTableHandler$connectionHandler$connectionDetails,
#       cohortTable = "cohort"
#     )
#
#     sql <- "-- This code inserts records from an external cohort table into the cohort table of the cohort database schema.
#             DELETE FROM @target_database_schema.@target_cohort_table where cohort_definition_id = @target_cohort_id;
#             INSERT INTO @target_database_schema.@target_cohort_table (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
#             SELECT cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
#             FROM main.cohort
#             WHERE cohort_definition_id = "
#
#     cohortDefinitionSet <- eunomiaCohorts |>
#       dplyr::transmute(
#         cohortId = as.double(cohortId),
#         cohortName = name,
#         sql = paste(sql, cohortId),
#         json = " "
#       )
#
#     cohortTableHandler$insertOrUpdateCohorts(cohortDefinitionSet)
#   }
#
#   return(cohortTableHandler)
#
# }
