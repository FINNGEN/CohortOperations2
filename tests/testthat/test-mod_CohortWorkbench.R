

test_that("mod_cohortWorkbench_server produces output", {

  databasesHandlers <- helper_createNewDatabaseHandlers(withEunomiaCohorts = TRUE)

  on.exit({rm(databasesHandlers);gc()})

  cohortsSummaryDatabases <- fct_getCohortsSummariesFromDatabasesHandlers(databasesHandlers)

  r_connectionHandlers <- shiny::reactiveValues(
    databasesHandlers = databasesHandlers
  )

  r_workbench <- shiny::reactiveValues(
    cohortsSummaryDatabases = cohortsSummaryDatabases
  )
  shiny::testServer(
    mod_cohortWorkbench_server,
    args = list(
      id = "test",
      r_connectionHandlers = r_connectionHandlers,
      r_workbench = r_workbench),
    {
        # expect output$cohortsSummaryDatabases_reactable not null
        expect_true(!is.null(output$cohortsSummaryDatabases_reactable))

    }
  )


})
