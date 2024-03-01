#' matchCohorts UI Function
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
mod_matchCohorts_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    mod_appendCohort_ui(),
    shinyjs::useShinyjs(),
    #
    shiny::tags$h4("Database"),
    shinyWidgets::pickerInput(
      inputId = ns("selectDatabases_pickerInput"),
      label = "Database (where to find matches from):",
      choices = NULL,
      selected = NULL,
      multiple = FALSE),
    htmltools::hr(),
    shiny::tags$h4("Cohorts"),
    shinyWidgets::pickerInput(
      inputId = ns("selectTargetCohort_pickerInput"),
      label = "Controls cohort (where to find matches from):",
      choices = NULL,
      selected = NULL,
      multiple = FALSE),
    shinyWidgets::pickerInput(
      inputId = ns("selectMatchCohort_pickerInput"),
      label = "Cases cohort (defining cohort):",
      choices = NULL,
      selected = NULL,
      multiple = FALSE),
    htmltools::hr(),
    shiny::tags$h4("Settings"),
    shiny::numericInput(
      inputId = ns("matchRatio_numericInput"),
      label = "Matching ratio for controls",
      value = 10,
      min = 1,
      max = 1000
    ),
    shinyWidgets::prettySwitch(
      inputId = ns("matchSex_switch"),
      label = "Match sex",
      status = "primary",
      value = TRUE
    ),
    shinyWidgets::prettySwitch(
      inputId = ns("matchBirthYear_switch"),
      label = "Match birth year",
      status = "primary",
      value = TRUE
    ),
    shinyWidgets::prettySwitch(
      inputId = ns("matchCohortStartDateWithInDuration_switch"),
      label = "Match cohort start date within duration",
      status = "primary",
      value = FALSE
    ),
    shinyWidgets::radioGroupButtons(
      inputId = ns("newCohortStartDate_option"),
      label = "Patients in new cohort to have cohort start date as in",
      choices = list(
        `Target cohort` = "keep",
        `Matched control cohort` = "asMatch"
      ),
      individual = TRUE,
      checkIcon = list(
        yes = shiny::tags$i(class = "fa fa-circle",
                     style = "color: steelblue"),
        no = shiny::tags$i(class = "fa fa-circle-o",
                    style = "color: steelblue"))
    ),
    shinyWidgets::radioGroupButtons(
      inputId = ns("newCohortEndDate_option"),
      label = "Patients in new cohort to have cohort end date as in",
      choices = list(
        `Target cohort` = "keep",
        `Matched control cohort` = "asMatch"
      ),
      individual = TRUE,
      checkIcon = list(
        yes = shiny::tags$i(class = "fa fa-circle",
                     style = "color: steelblue"),
        no = shiny::tags$i(class = "fa fa-circle-o",
                    style = "color: steelblue"))
    ),
    #
    htmltools::hr(),
    shiny::tags$h4("Summary"),
    shiny::textOutput(ns("newCohortName_text")),
    shiny::tags$br(),
    shiny::actionButton(ns("create_actionButton"), "Create matching cohort")

  )
}

#' import_cohort_file Server Functions
#'



mod_matchCohorts_server <- function(id, r_connectionHandlers, r_workbench) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      cohortDefinitionSet = NULL
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
    # update selectTargetCohort_pickerInput with cohort names in selectDatabases_pickerInput database
    #
    shiny::observe({
      shiny::req(r_workbench$cohortsSummaryDatabases)
      shiny::req(input$selectDatabases_pickerInput)

      cohortIdAndNames <- r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandler$getCohortIdAndNames()
      cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, cohortIdAndNames$cohortName))

      shinyWidgets::updatePickerInput(
        inputId = "selectTargetCohort_pickerInput",
        choices = cohortIdAndNamesList,
        selected = character(0)
      )
    })


    #
    # update matchToCohortId_pickerInput with cohort names not in selectTargetCohort_pickerInput
    #
    shiny::observe({
      shiny::req(r_workbench$cohortsSummaryDatabases)
      shiny::req(input$selectDatabases_pickerInput)
      shiny::req(input$selectTargetCohort_pickerInput)

      cohortTableHandler <- r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandler

      if(input$selectTargetCohort_pickerInput != "NA"){
        cohortIdAndNames <- cohortTableHandler$getCohortIdAndNames() |>
          dplyr::filter(!(cohortId %in% input$selectTargetCohort_pickerInput))
        cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, cohortIdAndNames$cohortName))
      }else{
        cohortIdAndNamesList <- list()
      }


      shinyWidgets::updatePickerInput(
        inputId = "selectMatchCohort_pickerInput",
        choices = cohortIdAndNamesList,
        selected = character(0)
      )
    })

    #
    # activate settings if cohors have been selected
    #
    shiny::observe({
      condition <- !is.null(input$selectMatchCohort_pickerInput) & input$selectMatchCohort_pickerInput!="NA"
      shinyjs::toggleState("matchRatio_numericInput", condition = condition )
      shinyjs::toggleState("matchSex_switch", condition = condition )
      shinyjs::toggleState("matchBirthYear_switch", condition = condition )
      shinyjs::toggleState("matchCohortStartDateWithInDuration_switch", condition = condition )
      shinyjs::toggleState("newCohortStartDate_option", condition = condition )
      shinyjs::toggleState("newCohortEndDate_option", condition = condition )
      shinyjs::toggleState("create_actionButton", condition = condition )
    })


    #
    # create temporal cohortDefinitionSet and render name
    #
    shiny::observe({
      shiny::req(input$selectDatabases_pickerInput)
      shiny::req(input$selectTargetCohort_pickerInput)
      shiny::req(input$selectTargetCohort_pickerInput!="NA")
      shiny::req(input$selectMatchCohort_pickerInput)
      shiny::req(input$selectMatchCohort_pickerInput!="NA")

      cohortTableHandler <- r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandler
#browser()
      existingSubsetDefinitionIds <- cohortTableHandler$cohortDefinitionSet |>
        dplyr::filter(!is.na(subsetDefinitionId)) |>
        dplyr::pull(subsetDefinitionId)

      nextSubsetDefinitionId <- ifelse( length(existingSubsetDefinitionIds) == 0, 1, max(existingSubsetDefinitionIds)+1 )


      # Match to sex and bday, match ratio 10
      subsetDef <- CohortGenerator::createCohortSubsetDefinition(
        name = "",
        definitionId = nextSubsetDefinitionId,
        subsetOperators = list(
          HadesExtras::createMatchingSubset(
            matchToCohortId = input$selectMatchCohort_pickerInput,
            matchRatio = input$matchRatio_numericInput,
            matchSex = input$matchSex_switch,
            matchBirthYear = input$matchBirthYear_switch,
            matchCohortStartDateWithInDuration = input$matchCohortStartDateWithInDuration_switch,
            newCohortStartDate = input$newCohortStartDate_option,
            newCohortEndDate = input$newCohortEndDate_option
          )
        )
      )

      cohortDefinitionSet <- CohortGenerator::addCohortSubsetDefinition(
        cohortDefinitionSet = cohortTableHandler$cohortDefinitionSet |> dplyr::mutate(cohortId=as.double(cohortId)),# TEMP FIX
        cohortSubsetDefintion = subsetDef,
        targetCohortIds = as.integer(input$selectTargetCohort_pickerInput),
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
      if(!shiny::isTruthy(r$cohortDefinitionSet)){
        "----"
      }else{
        r$cohortDefinitionSet$cohortName
      }
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





















