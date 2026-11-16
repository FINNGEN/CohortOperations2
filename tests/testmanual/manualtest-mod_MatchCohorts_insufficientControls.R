# Manual local-host test for the insufficient-controls confirmation flow
#
# Run this file from the repository root in an interactive R session:
#   source("tests/testmanual/manualtest-mod_MatchCohorts_insufficientControls.R")
#
# The test uses the same real test database/cohort fixtures as the existing
# matching manual test. It deliberately asks for an unrealistically high
# matching ratio so that the confirmation modal is easy to trigger.

devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

fcr_setUpLogger()

cohortTableHandler <- helper_createNewCohortTableHandler(
  addCohorts = "HadesExtrasFractureCohorts"
)

app <- shiny::shinyApp(
  shiny::fluidPage(
    shiny::tags$h3("Manual test: insufficient controls during matching"),
    shiny::tags$ol(
      shiny::tags$li("Select any case cohort and control cohort in the matching module."),
      shiny::tags$li("Set the requested matching ratio to a value much larger than the available control pool, for example 10000."),
      shiny::tags$li("Confirm that the pre-run information shows the requested controls and maximum average ratio."),
      shiny::tags$li("Click Create matching cohort and confirm that the warning modal appears."),
      shiny::tags$li("Click Cancel and verify that no new cohort is added."),
      shiny::tags$li("Repeat the action and click Continue with available controls; verify that the matching cohort is then created."),
      shiny::tags$li("Repeat with a ratio that allows at least one case to be retained, choose Randomly subsample cases, and confirm the second dialog."),
      shiny::tags$li("Verify that both the deterministic case subset and matched control cohort are created.")
    ),
    shiny::tags$p(
      "Case subsampling uses the fixed seed shown in the confirmation dialog. " ,
      "The requested ratio is never silently changed."
    ),
    htmltools::hr(),
    mod_cohortWorkbench_ui("test"),
    mod_matchCohorts_ui("test")
  ),
  function(input, output, session) {
    r_databaseConnection <- shiny::reactiveValues(
      cohortTableHandler = cohortTableHandler,
      atlasConfig = NULL,
      hasChangeCounter = 0
    )

    mod_matchCohorts_server("test", r_databaseConnection)
    mod_cohortWorkbench_server("test", r_databaseConnection)
  },
  options = list(launch.browser = TRUE)
)

app
