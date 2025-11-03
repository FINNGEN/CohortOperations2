
helper_createNewCohortTableHandler <- function(addCohorts = NULL){

  addCohorts |> checkmate::assertCharacter(len = 1, null.ok = TRUE)
  addCohorts |> checkmate::assertSubset(c(
    "EunomiaDefaultCohorts", "HadesExtrasFractureCohorts","HadesExtrasAsthmaCohorts",
    "HadesExtrasFractureCohortsMatched","HadesExtrasAsthmaCohortsMatched"
  ), empty.ok = TRUE)

  # by default use the one from setup.R
  cohortTableHandlerConfig <- test_cohortTableHandlerConfig # set by setup.R

  loadConnectionChecksLevel = "basicChecks"

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


 helper_addCohortAndCohortDefinitionTables <- function(cohortTableHandlerConfig, cohortTablesToAdd = "Diabetes"){

  connectionDetailsSettings <- cohortTableHandlerConfig$connection$connectionDetailsSettings
  connectionDetails <- rlang::exec(DatabaseConnector::createConnectionDetails, !!!connectionDetailsSettings)
  connection  <- DatabaseConnector::connect(connectionDetails)

  if (cohortTablesToAdd == "Diabetes") {

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
      2, 6, as.Date("2000-01-01"), as.Date("2010-12-01"),

      4, 1, as.Date("2000-01-01"), as.Date("2000-12-01"),
      4, 2, as.Date("2000-01-01"), as.Date("2000-12-01"),
      4, 3, as.Date("2000-01-01"), as.Date("2000-12-01"),
      4, 4, as.Date("2000-01-01"), as.Date("2000-12-01"),
      4, 5, as.Date("2000-01-01"), as.Date("2000-12-01"),
      4, 5, as.Date("2004-01-01"), as.Date("2004-12-01"),

      5, 2, as.Date("2001-01-01"), as.Date("2002-12-01"),# non overplaping
      5, 3, as.Date("2000-06-01"), as.Date("2000-09-01"),# inside
      5, 4, as.Date("2000-06-01"), as.Date("2010-12-01"),# overlap
      5, 5, as.Date("2004-06-01"), as.Date("2010-12-01"),# overlap with second
      5, 6, as.Date("2000-01-01"), as.Date("2010-12-01")
    )

    testCohortDefinitionTable <- tibble::tribble(
      ~cohort_definition_id, ~cohort_definition_name, ~cohort_definition_description, ~definition_type_concept_id, ~cohort_definition_syntax, ~subject_concept_id, ~cohort_initiation_date,
      1, 'Diabetes Cohort', 'Cohort of patients diagnosed with diabetes', 1234, 'SELECT * FROM patients WHERE diagnosis = "Diabetes"', 5678, as.Date('2022-01-01'),
      2, 'Hypertension Cohort', 'Cohort of patients diagnosed with hypertension', 1234, 'SELECT * FROM patients WHERE diagnosis = "Hypertension"', 5678, as.Date('2022-01-01'),
      3, 'Obesity Cohort', 'Cohort of patients diagnosed with obesity', 1234, 'SELECT * FROM patients WHERE diagnosis = "Obesity"', 5678, as.Date('2022-01-01'),
      #
      4, 'Hypertension Cohort lib [CohortLibrary]', 'Cohort of patients diagnosed with hypertension', 1234, 'SELECT * FROM patients WHERE diagnosis = "Hypertension"', 5678, as.Date('2022-01-01'),
      5, 'Obesity Cohort lib [CohortLibrary]', 'Cohort of patients diagnosed with obesity', 1234, 'SELECT * FROM patients WHERE diagnosis = "Obesity"', 5678, as.Date('2022-01-01')
    )
  }

  DatabaseConnector::insertTable(
    connection = connection,
    table = "cohort",
    data = testCohortTable
  )

  DatabaseConnector::insertTable(
    connection = connection,
    table = "cohort_definition",
    data = testCohortDefinitionTable
  )
}


helper_FinnGen_getDatabaseFile <- function(){
   if( Sys.getenv("EUNOMIA_DATA_FOLDER") == "" ){
    message("EUNOMIA_DATA_FOLDER not set. Please set this environment variable to the path of the Eunomia data folder.")
    stop()
  }

  urlToFinnGenEunomiaZip <- "https://raw.githubusercontent.com/FINNGEN/EunomiaDatasets/main/datasets/FinnGenR12/FinnGenR12_v5.4.zip"
  eunomiaDataFolder <- Sys.getenv("EUNOMIA_DATA_FOLDER")

  # Download the database if it doesn't exist
  if (!file.exists(file.path(eunomiaDataFolder, "FinnGenR12_v5.4.zip")) | !file.exists(file.path(eunomiaDataFolder, "FinnGenR12_v5.4.sqlite"))){

    result <- utils::download.file(
      url = urlToFinnGenEunomiaZip,
      destfile = file.path(eunomiaDataFolder, "FinnGenR12_v5.4.zip"),
      mode = "wb"
    )

    Eunomia::extractLoadData(
      from = file.path(eunomiaDataFolder, "FinnGenR12_v5.4.zip"),
      to = file.path(eunomiaDataFolder, "FinnGenR12_v5.4.sqlite"),
      cdmVersion = '5.4',
      verbose = TRUE
    )
  }

  # copy to a temp folder
  file.copy(
    from = file.path(eunomiaDataFolder, "FinnGenR12_v5.4.sqlite"),
    to = file.path(tempdir(), "FinnGenR12_v5.4.sqlite"),
    overwrite = TRUE
  )

  return(file.path(tempdir(), "FinnGenR12_v5.4.sqlite"))
}



