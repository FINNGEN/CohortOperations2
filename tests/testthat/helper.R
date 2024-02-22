

helper_createNewDatabaseHandlers <- function(withEunomiaCohorts = FALSE) {

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
