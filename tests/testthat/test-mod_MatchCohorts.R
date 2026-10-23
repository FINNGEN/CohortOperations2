
test_that("matching feasibility identifies insufficient controls", {
  feasibility <- .assess_matching_feasibility(
    n_cases = 100,
    n_controls = 250,
    requested_ratio = 5
  )

  expect_false(feasibility$sufficient_controls)
  expect_equal(feasibility$requested_controls, 500)
  expect_equal(feasibility$available_average_ratio, 2.5)
})


test_that("matching feasibility identifies sufficient controls", {
  feasibility <- .assess_matching_feasibility(
    n_cases = 100,
    n_controls = 500,
    requested_ratio = 5
  )

  expect_true(feasibility$sufficient_controls)
  expect_equal(feasibility$requested_controls, 500)
  expect_equal(feasibility$available_average_ratio, 5)
})


test_that("case subsampling falls back to 1:1 when controls are fewer than cases", {
  subsampling <- .assess_matching_subsampling(
    n_cases = 56,
    n_controls = 5,
    requested_ratio = 100
  )

  expect_equal(subsampling$subsample_ratio, 1)
  expect_equal(subsampling$maximum_subsampled_cases, 5)
  expect_true(subsampling$subsample_cases_possible)
})


test_that("case subsampling keeps the requested ratio when controls are sufficient for 1:1", {
  subsampling <- .assess_matching_subsampling(
    n_cases = 56,
    n_controls = 2638,
    requested_ratio = 100
  )

  expect_equal(subsampling$subsample_ratio, 100)
  expect_equal(subsampling$maximum_subsampled_cases, 26)
  expect_true(subsampling$subsample_cases_possible)
})


test_that("mod_MatchCohorts produces output", {

  cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
  withr::defer({cohortTableHandler$closeConnection();rm(cohortTableHandler);gc()})

  r_databaseConnection <- shiny::reactiveValues(
    cohortTableHandler = cohortTableHandler,
    hasChangeCounter = 0
  )

  shiny::testServer(
    mod_matchCohorts_server,
    args = list(
      id = "test",
      r_databaseConnection = r_databaseConnection
    ),
    {
      # initial state
      # set inputs
      session$setInputs(
        selectControlCohort_pickerInput = NULL,
        selectCaseCohort_pickerInput = NULL,
        matchRatio_numericInput = 5,
        matchSex_switch = TRUE,
        matchBirthYear_switch = TRUE,
        matchCohortStartDateWithInDuration_switch = FALSE,
        newCohortStartDate_option = "asMatch",
        newCohortEndDate_option = "asMatch"
      )

      r_databaseConnection$cohortTableHandler$getCohortIdAndNames() |> dplyr::pull(cohortId) |>
        expect_equal(c(1,2))

      # set inputs and click add
      session$setInputs(
        selectControlCohort_pickerInput = 2,
        selectCaseCohort_pickerInput = 1,
        create_actionButton = 1
      )

      # test output
      r_databaseConnection$cohortTableHandler$getCohortIdAndNames() |> dplyr::pull(cohortId) |>
        expect_equal(c(1,2,3))

    }
  )

})


test_that("mod_MatchCohorts info text ", {

  cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasFractureCohorts")
  withr::defer({cohortTableHandler$closeConnection();rm(cohortTableHandler);gc()})

  r_databaseConnection <- shiny::reactiveValues(
    cohortTableHandler = cohortTableHandler,
    hasChangeCounter = 0
  )

  shiny::testServer(
    mod_matchCohorts_server,
    args = list(
      id = "test",
      r_databaseConnection = r_databaseConnection
    ),
    {
      # initial state
      # set inputs
      session$setInputs(
        selectControlCohort_pickerInput = NULL,
        selectCaseCohort_pickerInput = NULL,
        matchRatio_numericInput = 5,
        matchSex_switch = TRUE,
        matchBirthYear_switch = TRUE,
        matchCohortStartDateWithInDuration_switch = FALSE,
        newCohortStartDate_option = "asMatch",
        newCohortEndDate_option = "asMatch"
      )

      # set inputs
      session$setInputs(
        selectControlCohort_pickerInput = 2,
        selectCaseCohort_pickerInput = 1
      )
      
      output$info_text |> expect_match("New cohort name")
      output$info_text |> expect_no_match("There are more subjects in matching/case cohort")
      output$info_text |> expect_match("No subjects overlap between matching/case and target/control")

      # set inputs
      session$setInputs(
        selectControlCohort_pickerInput = 1,
        selectCaseCohort_pickerInput = 2
      )

      output$info_text |> expect_match("There are more subjects in matching/case cohort")
    }
  )

})


test_that("mod_MatchCohorts info text 2", {

  cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "HadesExtrasAsthmaCohorts")
  withr::defer({cohortTableHandler$closeConnection();rm(cohortTableHandler);gc()})

  r_databaseConnection <- shiny::reactiveValues(
    cohortTableHandler = cohortTableHandler,
    hasChangeCounter = 0
  )

  shiny::testServer(
    mod_matchCohorts_server,
    args = list(
      id = "test",
      r_databaseConnection = r_databaseConnection
    ),
    {
      # initial state
      # set inputs
      session$setInputs(
        selectControlCohort_pickerInput = NULL,
        selectCaseCohort_pickerInput = NULL,
        matchRatio_numericInput = 5,
        matchSex_switch = TRUE,
        matchBirthYear_switch = TRUE,
        matchCohortStartDateWithInDuration_switch = FALSE,
        newCohortStartDate_option = "asMatch",
        newCohortEndDate_option = "asMatch"
      )

      # set inputs
      session$setInputs(
        selectControlCohort_pickerInput = 2,
        selectCaseCohort_pickerInput = 1
      )

      output$info_text |> expect_match("overlap between matching/case and target/control cohorts")
    }
  )

})
