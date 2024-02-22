
setup_ModalWithLog <- function(
    logFileName = tempfile(fileext = ".co2log.txt")
) {
  # setup  ------------------------------------------------------------------
  # init future
  future::plan(future::multisession, workers = 2)

  # init loger
  logger <- ParallelLogger::createLogger(
    appenders = list(ParallelLogger::createFileAppender(
      fileName = logFileName,
      layout = ParallelLogger::layoutSimple))
  )
  ParallelLogger::clearLoggers()
  ParallelLogger::registerLogger(logger)
  ParallelLogger::logTrace("Start logging")

  return(logger)
}




modalWithLog_server <- function(id,.f,.r_l, logger, logUpdateSeconds = 0.5, logLines=10) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # set up future
    queue <- ipc::shinyQueue()
    queue$consumer$start(100) # Execute signals every 100 milliseconds
    inter <- ipc::AsyncInterruptor$new()

    # set up modal
    autoUpdate <- shiny::reactiveTimer(logUpdateSeconds*1000)

    # intermediary result
    result_val <- shiny::reactiveVal()

    # when params update run function
    shiny::observeEvent(.r_l$.l,{
      .l = .r_l$.l
      ParallelLogger::logInfo("Launching future in modalWithLog_server id = ", id)
      #browser()
      future::future({
        # run function
        result <- NULL
        tryCatch({
          # register logger
          ParallelLogger::registerLogger(logger)
          ParallelLogger::logInfo("Launching ")
          #run
          result <- do.call(.f, .l)
          result <- list(
            success = TRUE,
            result = result
          )
          #
        }, error = function(e){
          ParallelLogger::logError("Error in future in modalWithLog_server", e)
          result <<- list(
            success = FALSE,
            result = e$message
          )
        })

        # set result to reactive
        queue$producer$fireAssignReactive("result_val",result)
        # set interrupter
        inter$execInterrupts()
      })

      shiny::showModal(shiny::modalDialog(
        shiny::tagList(
          ui_load_spinner(shiny::plotOutput(outputId = "plot", width = "500px", height = "100px"), proxy.height = "90px"),
          shiny::verbatimTextOutput(ns("modalContent"))
        ),
        footer = shiny::tagList(
          shiny::actionButton(ns("interrupt"), "Interrupt")
        ),
        size = "xl"
      ))
      #Return something other than the future so we don't block the UI
      NULL
    })

    # Update the modal content every second
    output$modalContent <- shiny::renderText({
      autoUpdate()
      file_path <- logger$appenders[[1]]$fileName

      lines <- readLines(file_path)
      last_lines <- tail(lines, logLines)

      paste0(last_lines, collapse = "\n")
    })


    # if clicked interrup, interrupt and close modal
    shiny::observeEvent(input$interrupt,{
      inter$interrupt("Stop that future")
      shiny::removeModal()
    })

    # when result ready close modal and return
    shiny::reactive({
      shiny::req(result_val())
      if(is.character(result_val())){
        return(result_val())
      }else{
        shiny::removeModal()
        return(result_val())
      }
    })

  })
}

