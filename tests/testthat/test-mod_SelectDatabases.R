

test_that("mod_cohortWorkbench_server updates r_databaseConnection when database selected", {

  testthat::skip_if_not(Sys.getenv("HADESEXTAS_TESTING_ENVIRONMENT") == "Eunomia-GiBleed")

  # setup
  r_databaseConnection <- shiny::reactiveValues(
    cohortTableHandler = NULL,
    atlasConfig = NULL,
    hasChangeCounter = 0
  )

  databasesConfig <- test_databasesConfig

  # run module
  shiny::testServer(
    mod_selectDatabases_server,
    args = list(
      id = "test",
      databasesConfig = databasesConfig,
      r_databaseConnection = r_databaseConnection
    ),
    {
      # inputs
      session$setInputs(
        selectDatabases_pickerInput = databasesConfig |> names() |> dplyr::first(),
        allChecks_checkbox = FALSE
      )

      # test
      output$connectionStatusLogs_reactable  |> expect_match('"databaseName":\\["GiBleed","GiBleed",')
      r_databaseConnection$hasChangeCounter |> expect_equal(1)
      r_databaseConnection$cohortTableHandler |> class() |> expect_contains("CohortTableHandler")
      r$connectionStatusLogs  |> dplyr::filter(step == "Check Atlas connection")  |> dplyr::pull(message) |> expect_equal("Connected")

      # inputs
      session$setInputs(
        selectDatabases_pickerInput = databasesConfig |> names() |> dplyr::nth(2),
        allChecks_checkbox = FALSE
      )

      # test
      output$connectionStatusLogs_reactable  |> expect_match('"databaseName":\\["MIMIC","MIMIC",')
      r_databaseConnection$hasChangeCounter |> expect_equal(2)
      r_databaseConnection$cohortTableHandler |> class() |> expect_contains("CohortTableHandler")
      r$connectionStatusLogs  |> dplyr::filter(step == "Check Atlas connection")  |> dplyr::pull(message) |> expect_equal("Connected")

      # inputs
      session$setInputs(
        selectDatabases_pickerInput = databasesConfig |> names() |> dplyr::nth(3),
        allChecks_checkbox = FALSE
      )

      # test
      output$connectionStatusLogs_reactable  |> expect_match('"databaseName":\\["MIMICwrong","MIMICwrong",')
      r_databaseConnection$hasChangeCounter |> expect_equal(3)
      r_databaseConnection$cohortTableHandler |> class() |> expect_contains("CohortTableHandler")
      r$connectionStatusLogs  |> dplyr::filter(step == "Check Atlas connection")  |> dplyr::pull(message) |> expect_match("Could not reach WebApi")

      # An unavailable database configuration must fail safely without leaving
      # a retired handler available to other modules.
      session$setInputs(
        selectDatabases_pickerInput = databasesConfig |> names() |> dplyr::nth(4),
        allChecks_checkbox = FALSE
      )

      r_databaseConnection$connectionState |> expect_equal("failed")
      r_databaseConnection$cohortTableHandler |> expect_null()
      r$connectionStatusLogs |> dplyr::filter(step == "Connect to database") |>
        dplyr::pull(message) |> expect_match("SQLite database file is missing")

    }
  )

})
