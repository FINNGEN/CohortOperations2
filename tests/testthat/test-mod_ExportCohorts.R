

test_that("mod_exportsCohorts produces file", {

  cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "EunomiaDefaultCohorts")
  withr::defer({rm(cohortTableHandler);gc()})

  r_databaseConnection <- shiny::reactiveValues(
    cohortTableHandler = cohortTableHandler,
    atlasConfig = NULL,
    hasChangeCounter = 0
  )

  # ignore warnings
  suppressWarnings({
  shiny::testServer(
    mod_exportsCohorts_server,
    args = list(
      id = "test",
      r_databaseConnection = r_databaseConnection
    ),
    {
      # select cohorts
      session$setInputs(
        selectCohorts_pickerInput = c(1,2),
        co1Compatible_checkbox = TRUE
      )
      
      r$selectedCohortsInfo |> dplyr::pull(cohortId) |> expect_equal(c(1,2))
      r$writeErrorMessage |> expect_null()

      # check file
      expect_true(file.exists(output$downloadData_downloadButton))

    }
    )
  })
})
