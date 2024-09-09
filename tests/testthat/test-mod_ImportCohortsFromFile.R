#
#
# test_that("mod_ImportCohortsFromFile produces output", {
#
#   cohortTableHandler <- helper_createNewCohortTableHandler(addCohorts = "EunomiaDefaultCohorts")
#   withr::defer({rm(cohortTableHandler);gc()})
#
#   r_databaseConnection <- shiny::reactiveValues(
#     cohortTableHandler = cohortTableHandler,
#     hasChangeCounter = 0
#   )
#
#   shiny::testServer(
#     mod_importCohortsFromFile_server,
#     args = list(
#       id = "test",
#       r_databaseConnection = r_databaseConnection
#     ),
#     {
#       browser()
#       # Test: initial state
#       expect_null(r$uploadedFile)
#       expect_null(r$cohortDataUploaded)
#       expect_null(r$cohortData)
#       expect_false(r$columnNamesOK)
#
#       # delete cohort
#       session$setInputs(
#         uploadedFile = list(
#           name = "cohortData_eunomia.csv",
#           size = 1234,
#           type = "text/csv",
#           datapath = testthat::test_path("testdata", "cohortData_eunomia.csv")
#         )
#       )
#
#       #????
#       r$uploadedFile <- input$uploadedFile
#       r$columnNamesOK <- TRUE
#
#       # Check that the file was processed
#       expect_equal(r$uploadedFile$name, "cohortData_eunomia.csv")
#       expect_true(r$columnNamesOK)
#
#       # Simulate user selecting cohorts in the reactable and clicking the import button
#       session$setInputs(cohorts_reactable__selected = c(1))  # Assume row 1 is selected
#       session$setInputs(import_actionButton = 1)
#
#       # Check that selected cohorts are being processed for import
#       expect_equal(length(r_cohortDefinitionSetToAdd$cohortDefinitionSet$cohortName), 1)
#       expect_equal(r_cohortDefinitionSetToAdd$cohortDefinitionSet$cohortName, "Test Cohort")
#
#
#     }
#   )
#
# })
