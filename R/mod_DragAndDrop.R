

mod_dragAndDrop_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::uiOutput(ns("operation_expression")),
    # shiny::textOutput(ns("dest_boxes")),
  )}


mod_dragAndDrop_server <- function(id, session, cohort_list) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    cohort_list <- c("COHORT1", "COHORT2", "COHORT3", "COHORT4", "COHORT5")

    output$operation_expression <- shiny::renderUI({

      htmltools::tagList(
        shinydashboard::box(
          title = "Expression defining the cohort",
          width = 12,
          background = "light-blue",
          shiny::textOutput(ns("select_patients_start_text")),
          shinyjqui::orderInput(
            inputId = ns("dest_boxes"),
            width = "100%",
            label = NULL,
            items = NULL, placeholder = "Drag and Drop COHORTS and OPERATORS here to create an expression",
          )
        ),
        shinydashboard::box(
          width = 12,
          shinyjqui::orderInput(
            inputId = ns("source_boxes_cohorts"),
            width = "100%",
            label = "COHORTS",
            items = cohort_list,
            as_source = TRUE, connect = ns("dest_boxes")
          ),
          shinyjqui::orderInput(
            inputId = ns("source_boxes"),
            width = "100%",
            label = "OPERATORS",
            items = c(`(`="(", `)`=")",
                      ` if they are also in `="&",
                      ` if they are not in `="&!",
                      ` or in ` = "|"),
            as_source = TRUE, connect = ns("dest_boxes")
          )
        )
      )
    })

    output$dest_boxes <- shiny::renderPrint({
      req(!is.null(input$dest_boxes))

      input$dest_boxes
    })
  })


  #
  # calculates cohort operation expression
  #
  shiny::reactive({

    operation_expression <- NULL
    if (!is.null(input$dest_boxes)) {
      operation_expression <- stringr::str_c(input$dest_boxes, collapse = "")
    }
    operation_expression
  })


}
