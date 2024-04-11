#' operateCohorts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS fileInput actionButton
#' @importFrom htmltools tagList hr
#' @importFrom shinyjs useShinyjs
#' @importFrom reactable reactableOutput
mod_operateCohorts_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    mod_appendCohort_ui(),
    shinyjs::useShinyjs(),
    #
    shiny::tags$h4("Database"),
    shiny::tags$h5("Select database where to work:"),
    shiny::tags$h6("(Only cohort in the same database can be matched)"),
    shinyWidgets::pickerInput(
      inputId = ns("selectDatabases_pickerInput"),
      label = NULL,
      choices = NULL,
      selected = NULL,
      multiple = FALSE),
    htmltools::hr(),
    shiny::tags$h4("Operation"),
    #shiny::textInput(inputId = ns("operationString_textInput"), label = "Operation String"),
    mod_dragAndDrop_ui(ns("dragAndDrop")),
    #
    htmltools::hr(),
    htmltools::div(style = "width: 100%; height: 300px; overflow: auto; margin: 20px; padding: 0px;",
      upsetjs::upsetjsOutput(ns("upsetjs1"), width = "90%", height = "300px"),
    ),
    htmltools::hr(),
    shiny::tags$h4("New cohort name"),
    shiny::textOutput(ns("newCohortName_text")),
    shiny::tags$br(),
    shiny::actionButton(ns("create_actionButton"), "Create new cohort")

  )
}

mod_operateCohorts_server <- function(id, r_connectionHandlers, r_workbench) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      cohortDefinitionSet = NULL,
      operationStringError = NULL
    )

    r_toAdd <- shiny::reactiveValues(
      databaseName = NULL,
      cohortDefinitionSet = NULL
    )


    #
    # update selectDatabases_pickerInput with database names
    #
    shiny::observe({
      shiny::req(r_connectionHandlers$databasesHandlers)

      databaseIdNamesList <- fct_getDatabaseIdNamesListFromDatabasesHandlers(r_connectionHandlers$databasesHandlers)

      shinyWidgets::updatePickerInput(
        inputId = "selectDatabases_pickerInput",
        choices = databaseIdNamesList,
        selected = databaseIdNamesList[1]
      )
    })


    #
    # get operation string from dragAndDrop module
    #
    rf_operationString <- mod_dragAndDrop_server("dragAndDrop", r_workbench)

    #
    # when operation string changes check if it is valid
    #
    shiny::observe({
      shiny::req(rf_operationString())

      error <- NULL
      tryCatch({
        HadesExtras::operationStringToSQL(rf_operationString())
      }, error = function(e){
        error <<- e$message
      })

      r$operationStringError <- error

    })


    #
    # if operation string is valid create temporal cohortDefinitionSet and render name
    #
    shiny::observe({
      shiny::req(input$selectDatabases_pickerInput)
      shiny::req(rf_operationString())
      shiny::req(is.null(r$operationStringError))

      cohortTableHandler <- r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandler

      existingSubsetDefinitionIds <- cohortTableHandler$cohortDefinitionSet |>
        dplyr::filter(!is.na(subsetDefinitionId)) |>
        dplyr::pull(subsetDefinitionId)

      nextSubsetDefinitionId <- ifelse( length(existingSubsetDefinitionIds) == 0, 1, max(existingSubsetDefinitionIds)+1 )

      subsetDef <- CohortGenerator::createCohortSubsetDefinition(
        name = "",
        definitionId = nextSubsetDefinitionId,
        subsetOperators = list(
          HadesExtras::createOperationSubset(
            operationString = rf_operationString()
          )
        )
      )

      # TEMP::get first integer From operation string
      targetCohortIds <- as.integer(stringr::str_extract(rf_operationString(), "\\d+"))

      cohortDefinitionSet <- CohortGenerator::addCohortSubsetDefinition(
        cohortDefinitionSet = cohortTableHandler$cohortDefinitionSet |> dplyr::mutate(cohortId=as.double(cohortId)),# TEMP FIX
        cohortSubsetDefintion = subsetDef,
        targetCohortIds = targetCohortIds,
        overwriteExisting =  TRUE
      )

      cohortDefinitionSet <- cohortDefinitionSet |>
        dplyr::filter(subsetDefinitionId == nextSubsetDefinitionId)

      r$cohortDefinitionSet <- cohortDefinitionSet

    })

    #
    # Render temporal name
    #
    output$newCohortName_text <- shiny::renderText({
      if(!is.null(r$operationStringError)){
        paste("❌", r$operationStringError)
      }else{
        if(!shiny::isTruthy(r$cohortDefinitionSet)){
          "----"
        }else{
          paste("✅", r$cohortDefinitionSet$cohortName)
        }
      }
    })

    #
    # create upset plot
    #
    # upset plot ####

    output$upsetjs1 <- upsetjs::renderUpsetjs({

      cohortTableHandler <- r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandler

      browser()

      cohortOverlap <- cohortTableHandler$getCohortsOverlap()


      dfInput <- tibble::tribble(
        ~one, ~two, ~three,
        1, 1, 1,
        1, 1, 0,
        1, 0, 0,
        0, 1, 0,
        1, 1, 1,
        0, 0, 1,
        1, 0, 1,
        1, 0, 1,
        0, 0, 1,
        0, 1, 1,
        1, 0, 0,
        1, 0, 1,
        1, 0, 1
      )

      upsetjs::upsetjs() |>
        upsetjs::fromDataFrame(dfInput) |>
        upsetjs::chartLayout(width.ratios = c(0.3, 0.1, 0.6)) |> # column proportions:  size, names, intersections (sum = 1.0)
        upsetjs::interactiveChart()
    })

    #
    # activate settings if cohors have been selected
    #
    shiny::observe({
      condition <- shiny::isTruthy(r$cohortDefinitionSet)
      shinyjs::toggleState("create_actionButton", condition = condition )
    })

    #
    # click to build cohort
    #
    shiny::observeEvent(input$create_actionButton, {
      shiny::req(r$cohortDefinitionSet)

      ## copy selected to
      r_toAdd$databaseName <- input$selectDatabases_pickerInput
      r_toAdd$cohortDefinitionSet <-  r$cohortDefinitionSet

    })

    #
    # evaluate the cohorts to append; if accepted increase output to trigger closing actions
    #
    r_append_accepted_counter <- mod_appendCohort_server("matchCohort", r_connectionHandlers, r_workbench, r_toAdd )

    # close and reset
    shiny::observeEvent(r_append_accepted_counter(), {
      # change in r_workbench$cohortsSummaryDatabases will update output$selectDatabases_pickerInput_uiOutput <- shiny::renderUI({
      # this will chain update the rest
      r$cohortDefinitionSet <- NULL
    })




  })
}





















