
# setup  ------------------------------------------------------------------

fcr_setUpLogger()
 
# run ---------------------------------------------------------------------
devtools::load_all(".")

ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  shiny::titlePanel("Countdown"),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::actionButton('run', 'count down')
    ),

    shiny::mainPanel(
      shiny::textOutput('result')
    )
  )
)

server <- function(input, output) {

  # When pressed button they start count down from 10 to 0
  shiny::observeEvent(input$run,{
    shinyjs::disable("run")

    fct_sweetAlertSpinner("Counting down")

    for(i in 10:0){
      Sys.sleep(1)
      ParallelLogger::logInfo(i)
    }
    shinyjs::enable("run")

    fct_removeSweetAlertSpinner()

  })



}


# Run the application
app <- shiny::shinyApp(
  ui = ui,
  server = server,
  options = list(
    port = 9999,
    launch.browser=TRUE
  )
)

cohortOperationsConfig <- yaml::read_yaml(testthat::test_path("config", "cohortOperationsConfig.yml"))
app$appOptions$cohortOperationsConfig  <- cohortOperationsConfig
app

