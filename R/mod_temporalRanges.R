
.mod_timeRange_ui <- function(id, startRange=c(0,0)) {
  ns <- shiny::NS(id)

  min <- -99999
  max <- 99999

  shiny::div(
    id = id,
    shiny::fluidRow(
      shiny::column(width = 1, shiny::numericInput(ns("lowRange"),  NULL, value = startRange[1], min = min, max = max, width = "100px")),
      shiny::column(width = 1, shiny::numericInput(ns("highRange"), NULL, value = startRange[2], min = min, max = max,  width = "100px")),
      shiny::column(width = 9,
                    shinyWidgets::sliderTextInput(
                      inputId = ns("barRange"),
                      label = NULL,
                      choices = min:max,
                      selected = startRange,
                      width = "500px"
                    )
      ),
      shiny::column(width = 1, shiny::actionButton(ns("remove"), shiny::icon("trash")))
    )
  )
}

.mod_timeRange_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    r <- shiny::reactiveValues(
      outputRange = NULL
    )

    shiny::observe({
      shiny::req(input$lowRange)
      shiny::req(input$highRange)
      shinyWidgets::updateSliderTextInput(session, "barRange", selected = c(input$lowRange,input$highRange))
    })

    shiny::observe({
      shiny::req(input$barRange)
      shiny::updateNumericInput(session, "lowRange", value = input$barRange[1])
      shiny::updateNumericInput(session, "highRange", value = input$barRange[2])
      r$outputRange <- input$barRange
    })

    shiny::observeEvent(input$remove,{
      # father tag
      selfId <- ns("") |>  stringr::str_sub(end = -2)
      shiny::removeUI(
        selector = paste0("#", selfId)
      )
      r$outputRange <- NULL
    })

    shiny::reactive(r$outputRange)

  })
}


mod_temporalRanges_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(id=ns('inputList')),
    shiny::actionButton(ns('addBtn'), 'Add Window')
  )
}


mod_temporalRanges_server <- function(id, session, startRanges = list(c(-99999,99999), c(99999,0))) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #listOutputRanges <- list()

    r <- shiny::reactiveValues(
      listOutputRanges = NULL
    )

    initFlag <- TRUE
    shiny::observe({
      shiny::req(initFlag)

      for (i in 1:length(startRanges)) {
        # sever module
        r$listOutputRanges[[i]] <<- .mod_timeRange_server(i)
        # ui module
        shiny::insertUI(
          selector = paste0('#', ns('inputList')),
          ui = .mod_timeRange_ui(ns(i), startRanges[[i]])
        )
      }

      initFlag <<- FALSE
    })




    shiny::observeEvent(input$addBtn, {
      i <- length(r$listOutputRanges)+1
      # sever module
      r$listOutputRanges[[i]] <<- .mod_timeRange_server(i)
      # ui module
      shiny::insertUI(
        selector = paste0('#', ns('inputList')),
        ui = .mod_timeRange_ui(ns(i))
      )
      # button action
      #browser()

    })

    shiny::reactive({

      temporalStartDays <- c()
      temporalEndDays <- c()

      listOutputRanges <- r$listOutputRanges

      if(length(listOutputRanges)!=0){
        for (i in 1:length(listOutputRanges)) {
          range <- listOutputRanges[[i]]()
          if(!is.null(range)){
            temporalStartDays <- c(temporalStartDays, range[1])
            temporalEndDays <- c(temporalEndDays, range[2])
          }
        }
      }

      list(
        temporalStartDays = temporalStartDays,
        temporalEndDays = temporalEndDays
      )
    })


  })
}
