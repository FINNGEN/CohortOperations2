

test_that("mod_cohortWorkbench_server updates r_connectionHandler when database selected", {

  # setup
  r_connectionHandler <- shiny::reactiveValues(
    cohortTableHandler = NULL,
    hasChangeCounter = 0
  )

  # run module
  shiny::testServer(
    mod_selectDatabases_server,
    args = list(
      id = "test",
      databasesConfig = databasesConfig,
      r_connectionHandler = r_connectionHandler
    ),
    {
      # inputs
      session$setInputs(
        selectDatabases_pickerInput = databasesConfig |> names() |> dplyr::first(),
        allChecks_checkbox = FALSE
      )

      # test
      output$connectionStatusLogs_reactable  |> expect_match('"databaseName":\\["GiBleed","GiBleed",')
      r_connectionHandler$hasChangeCounter |> expect_equal(1)
      r_connectionHandler$cohortTableHandler |> class() |> expect_contains("CohortTableHandler")

      # inputs
      session$setInputs(
        selectDatabases_pickerInput = databasesConfig |> names() |> dplyr::nth(2),
        allChecks_checkbox = FALSE
      )

      # test
      output$connectionStatusLogs_reactable  |> expect_match('"databaseName":\\["MIMIC","MIMIC",')
      r_connectionHandler$hasChangeCounter |> expect_equal(2)
      r_connectionHandler$cohortTableHandler |> class() |> expect_contains("CohortTableHandler")


    }
  )

})
