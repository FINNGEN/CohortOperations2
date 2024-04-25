
# setup  ------------------------------------------------------------------
folderWithLog <- file.path(tempdir(), "logs")
dir.create(folderWithLog, showWarnings = FALSE)
logger <- ParallelLogger::createLogger(
  appenders = list(
    # to console for traking
    ParallelLogger::createConsoleAppender(
      layout = .layoutParallelWithHeader
    ),
    # to file for showing in app
    ParallelLogger::createFileAppender(
      fileName = file.path(folderWithLog, "log.txt"),
      layout = ParallelLogger::layoutSimple
    )
  )
)
ParallelLogger::clearLoggers()
ParallelLogger::registerLogger(logger)
ParallelLogger::logTrace("Start logging")

shiny::addResourcePath("logs", folderWithLog)


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

    sweetAlert_spinner("Counting down")

    for(i in 10:0){
      Sys.sleep(1)
      ParallelLogger::logInfo(i)
    }
    shinyjs::enable("run")

    remove_sweetAlert_spinner()

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

