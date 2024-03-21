

helper_createNewDatabaseHandlers <- function(withEunomiaCohorts = FALSE, withCohortTableCohorts = FALSE) {

  databasesHandlers <- fct_databasesConfigToDatabasesHandlers(databasesConfig)

  if(withEunomiaCohorts==TRUE){

    cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
      settingsFileName = "testdata/name/Cohorts.csv",
      jsonFolder = "testdata/name/cohorts",
      sqlFolder = "testdata/name/sql/sql_server",
      cohortFileNameFormat = "%s",
      cohortFileNameValue = c("cohortName"),
      packageName = "CohortGenerator",
      verbose = FALSE
    )

    for (databaseId in names(databasesHandlers)) {
      databasesHandlers[[databaseId]]$cohortTableHandler$insertOrUpdateCohorts(cohortDefinitionSet)
    }

  }

  if(withCohortTableCohorts==TRUE){

    testCohortTable <-  tibble::tribble(
      ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
      1, 1, as.Date("2000-01-01"), as.Date("2000-12-01"),
      1, 2, as.Date("2000-01-01"), as.Date("2000-12-01"),
      1, 3, as.Date("2000-01-01"), as.Date("2000-12-01"),
      1, 4, as.Date("2000-01-01"), as.Date("2000-12-01"),
      1, 5, as.Date("2000-01-01"), as.Date("2000-12-01"),
      1, 5, as.Date("2004-01-01"), as.Date("2004-12-01"),

      2, 2, as.Date("2001-01-01"), as.Date("2002-12-01"),# non overplaping
      2, 3, as.Date("2000-06-01"), as.Date("2000-09-01"),# inside
      2, 4, as.Date("2000-06-01"), as.Date("2010-12-01"),# overlap
      2, 5, as.Date("2004-06-01"), as.Date("2010-12-01"),# overlap with second
      2, 6, as.Date("2000-01-01"), as.Date("2010-12-01")
    )

    testCohortDefinitionTable <- tibble::tribble(
      ~cohort_definition_id, ~cohort_definition_name, ~cohort_definition_description, ~definition_type_concept_id, ~cohort_definition_syntax, ~subject_concept_id, ~cohort_initiation_date,
      1, 'Diabetes Cohort', 'Cohort of patients diagnosed with diabetes', 1234, 'SELECT * FROM patients WHERE diagnosis = "Diabetes"', 5678, as.Date('2022-01-01'),
      2, 'Hypertension Cohort', 'Cohort of patients diagnosed with hypertension', 1234, 'SELECT * FROM patients WHERE diagnosis = "Hypertension"', 5678, as.Date('2022-01-01'),
      3, 'Obesity Cohort', 'Cohort of patients diagnosed with obesity', 1234, 'SELECT * FROM patients WHERE diagnosis = "Obesity"', 5678, as.Date('2022-01-01')
    )

    for (databaseId in names(databasesHandlers)) {

      DatabaseConnector::insertTable(
        connection = databasesHandlers[[databaseId]]$cohortTableHandler$connectionHandler$getConnection(),
        table = "cohort",
        data = testCohortTable
      )

      DatabaseConnector::insertTable(
        connection = databasesHandlers[[databaseId]]$cohortTableHandler$connectionHandler$getConnection(),
        table = "cohort_definition",
        data = testCohortDefinitionTable
      )
    }

  }


  return(databasesHandlers)

}


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
