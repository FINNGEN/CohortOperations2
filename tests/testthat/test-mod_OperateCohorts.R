

test_that("mod_OperateCohorts produces output", {

  cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "EunomiaDefaultCohorts")
  withr::defer({rm(cohortTableHandler);gc()})

  r_databaseConnection <- shiny::reactiveValues(
    cohortTableHandler = cohortTableHandler,
    atlasConfig = NULL,
    hasChangeCounter = 0
  )

  shiny::testServer(
    mod_operateCohorts_server,
    args = list(
      id = "test",
      r_databaseConnection = r_databaseConnection
    ),
    {
      # initial state
      session$flushReact()
      r_databaseConnection$cohortTableHandler$getCohortIdAndNames() |> dplyr::pull(cohortId) |>
        expect_equal(c(1,2,3,4))
      output$newCohortName_text  |> expect_match("----")
      output$upsetPlot  |> class() |> expect_equal("list")

      # valid operation string
      r$operationString <- "1Mp"
      session$flushReact()

      # test output
      output$newCohortName_text  |> expect_match("Operations must have atleast 3 elements")
      output$upsetPlot  |> class() |> expect_equal("list")

      # valid operation string
      r$operationString <- "1Mp3"
      suppressWarnings(
        session$flushReact()
      )

      # test output
      output$newCohortName_text  |> expect_match("Operation: 1Mp3")
      output$upsetPlot  |> class() |> expect_equal("list")

      # set inputs and click add
      session$setInputs(
        create_actionButton = 1
      )

      # test output
      r_databaseConnection$cohortTableHandler$getCohortIdAndNames() |> dplyr::pull(cohortId) |>
        expect_equal(c(1,2,3,4,5))

    }
  )

})
