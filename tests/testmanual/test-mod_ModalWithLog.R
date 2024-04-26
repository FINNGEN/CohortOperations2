
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

  shiny::titlePanel("Countdown"),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::actionButton('run', 'count down')
    ),

    shiny::mainPanel(
      shiny::tableOutput("result")
    )
  )
)

server <- function(input, output) {

  # A reactive value with the inputs to modalWithLog_server
  .r_l <- shiny::reactiveValues(
    .l = NULL
  )

  # When pressed button they update
  shiny::observeEvent(input$run,{
    # set parameters
    .r_l$.l <- list(
      a = 1,
      b = 2
    )
  })

  # Take parameters, run function in a future, open modal with log, close modal when ready, return value
  result_val <- modalWithLog_server(
    id = "sss",
    .f = function(a,b){
      #browser()
      for(i in 15:0){
        Sys.sleep(1)
        ParallelLogger::logInfo("Hello external f", i, a, b)
        # force error
        if(i==5) stop("force error")
        inter$execInterrupts()
      }
      return(tibble::tibble(x = a*b))
    },
    .r_l = .r_l,
    logger = logger)


  # set output to reactive value, wait for result , wait for job
  output$result <- shiny::renderTable({
    req(result_val())
    shiny::removeModal()
    result_val()
  })

}

# Run the application
shiny::shinyApp(ui = ui, server = server,
                options = list(launch.browser=TRUE))
