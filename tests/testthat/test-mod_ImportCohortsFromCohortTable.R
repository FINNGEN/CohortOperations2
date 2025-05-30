

test_that("mod_ImportCohortsFromCohortTable produces output", {

  cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "EunomiaDefaultCohorts")
  withr::defer({rm(cohortTableHandler);gc()})

  r_databaseConnection <- shiny::reactiveValues(
    cohortTableHandler = cohortTableHandler,
    hasChangeCounter = 0
  )

  shiny::testServer(
    mod_importCohortsFromCohortsTable_server,
    args = list(
      id = "test",
      r_databaseConnection = r_databaseConnection
    ),
    {
      # Test: initial state
      #r$cohortDefinitionTable  |> dplyr::pull(cohort_definition_name) |> expect_equal(c("Diabetes Cohort", "Hypertension Cohort", "Obesity Cohort"))
      output$cohorts_reactable |> expect_match('["Diabetes Cohort","Hypertension Cohort","Obesity Cohort"]')

      # select firts cohort
      r$selectedIndex <- 1
      # click import
      session$setInputs(
        import_actionButton = 2
      )
      
      # test output
      r_cohortDefinitionSetToAdd$cohortDefinitionTable |> expect_null()
      r_databaseConnection$cohortTableHandler$cohortDefinitionSet$cohortId |> expect_equal(c(1, 2, 3, 4, 5))

    }
  )

})


test_that("mod_ImportCohortsFromCohortTable shows error when no cohort table exists", {

  testthat::skip_if_not(Sys.getenv("HADESEXTAS_TESTING_ENVIRONMENT") == "Eunomia-GiBleed")

  cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "EunomiaDefaultCohorts")
  cohortTableHandlerConfig <- test_cohortTableHandlerConfig
  withr::defer({rm(cohortTableHandler);gc()})

  # delete cohort and cohort_definition tables
  cohortTableHandler$connectionHandler$getConnection() |> DatabaseConnector::dbExecute("DROP TABLE cohort")
  cohortTableHandler$connectionHandler$getConnection() |> DatabaseConnector::dbExecute("DROP TABLE cohort_definition")
  withr::defer({
    helper_addCohortAndCohortDefinitionTables(cohortTableHandlerConfig, cohortTablesToAdd = "Diabetes")
  })

  r_databaseConnection <- shiny::reactiveValues(
    cohortTableHandler = cohortTableHandler,
    hasChangeCounter = 0
  )

  shiny::testServer(
    mod_importCohortsFromCohortsTable_server,
    args = list(
      id = "test",
      r_databaseConnection = r_databaseConnection
    ),
    {
      # Test: initial state
      #r$cohortDefinitionTable  |> dplyr::pull(cohort_definition_name) |> expect_equal(c("Diabetes Cohort", "Hypertension Cohort", "Obesity Cohort"))
      output$cohorts_reactable  |> expect_error("Error connecting to Endpoint table.")

    }
  )

})
