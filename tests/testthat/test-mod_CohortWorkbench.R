

test_that("mod_cohortWorkbench_server produces output", {

  cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "EunomiaDefaultCohorts")
  withr::defer({rm(cohortTableHandler);gc()})

  r_connectionHandler <- shiny::reactiveValues(
    cohortTableHandler = cohortTableHandler,
    hasChangeCounter = 0
  )

  shiny::testServer(
    mod_cohortWorkbench_server,
    args = list(
      id = "test",
      r_connectionHandler = r_connectionHandler
    ),
    {

        # expect output$cohortsSummaryDatabases_reactable not null
      output$cohortsSummaryDatabases_reactable |> class()  |> expect_equal("json")
      output$cohortsSummaryDatabases_reactable |> expect_match("C1<br>celecoxib")
      output$cohortsSummaryDatabases_reactable |> expect_match("C2<br>celecoxibAge40")
      output$cohortsSummaryDatabases_reactable |> expect_match("C3<br>celecoxibAge40Male")

      # delete cohort
      session$setInputs(
        cohortsWorkbenchDeleteButtons = list(index = 1),
        confirmSweetAlert_CohortsWorkbenchDeleteButtons = TRUE
      )

      output$cohortsSummaryDatabases_reactable |> class()  |> expect_equal("json")
      output$cohortsSummaryDatabases_reactable |> expect_no_match("C1<br>celecoxib")
      output$cohortsSummaryDatabases_reactable |> expect_match("C2<br>celecoxibAge40")
      output$cohortsSummaryDatabases_reactable |> expect_match("C3<br>celecoxibAge40Male")

      # # Update cohort
      # session$setInputs(
      #   cohortsWorkbenchEditButtons = list(index = 2),
      #   editShortName_textInput = "AA",
      #   editCohortName_textInput = "BB",
      #   editCohort_actionButton = 1
      # )
      #
      # output$cohortsSummaryDatabases_reactable |> class()  |> expect_equal("json")
      # output$cohortsSummaryDatabases_reactable |> expect_no_match("C1<br>celecoxib")
      # output$cohortsSummaryDatabases_reactable |> expect_match("AA<br>BB")
      # output$cohortsSummaryDatabases_reactable |> expect_match("C3<br>celecoxibAge40Male")

    }
  )

})
