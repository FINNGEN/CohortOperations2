#' matchCohorts UI Function
#'
#' @description A shiny Module.
#'
#' @param id A module id.
#'
#' @importFrom shiny NS fileInput actionButton
#' @importFrom htmltools tagList hr
#' @importFrom shinyjs useShinyjs
#' @importFrom reactable reactableOutput
#'
#' @export
mod_matchCohorts_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    mod_fct_appendCohort_ui(),
    shinyjs::useShinyjs(),
    shiny::tags$h5("Create a new cohort, picking subjects from target/control cohort, with same characteristic as in matching/case cohort"),
    htmltools::hr(),
    shiny::tags$h4("Cohorts"),
    shinyWidgets::pickerInput(
      inputId = ns("selectCaseCohort_pickerInput"),
      label = "Select matching/case cohort:",
      choices = NULL,
      selected = NULL,
      multiple = FALSE),
    shinyWidgets::pickerInput(
      inputId = ns("selectControlCohort_pickerInput"),
      label = "Select target/control cohort:",
      choices = NULL,
      selected = NULL,
      multiple = FALSE),
    htmltools::hr(),
    shiny::tags$h4("Settings"),
    shiny::tags$h5("The new cohort will have a maximum of"),
    shiny::numericInput(
      inputId = ns("matchRatio_numericInput"),
      label = NULL,
      value = 10,
      min = 1,
      max = 10000
    ),
    shiny::tags$h5("subjects for each subject in matching/case cohort,"),
    shiny::tags$h5(" with"),
    shinyWidgets::prettySwitch(
      inputId = ns("matchSex_switch"),
      label = "Same Sex",
      status = "primary",
      value = TRUE
    ),
    shinyWidgets::prettySwitch(
      inputId = ns("matchBirthYear_switch"),
      label = "Same Birth Year",
      status = "primary",
      value = TRUE
    ),
    shinyWidgets::prettySwitch(
      inputId = ns("matchCohortStartDateWithInDuration_switch"),
      label = "Start date within target's/control's observation period",
      status = "primary",
      value = FALSE
    ),
    shinyWidgets::radioGroupButtons(
      inputId = ns("newCohortStartDate_option"),
      label = "Cohort start date in the new cohort will be ",
      choices = list(
        `unchange, same as the picked subjects from target/control cohort` = "keep",
        `match to be the same as in matching/case cohort` = "asMatch"
      ),
      direction = "vertical",
      selected = "asMatch",
      individual = TRUE,
      checkIcon = list(
        yes = shiny::tags$i(class = "fa fa-circle",
                            style = "color: steelblue"),
        no = shiny::tags$i(class = "fa fa-circle-o",
                           style = "color: steelblue"))
    ),
    shinyWidgets::radioGroupButtons(
      inputId = ns("newCohortEndDate_option"),
      label = "Cohort end date in the new cohort will be ",
      choices = list(
        `unchange, same as the picked subjects from target/control cohort` = "keep",
        `match to be the same as in matching/case cohort` = "asMatch"
      ),
      direction = "vertical",
      individual = TRUE,
      checkIcon = list(
        yes = shiny::tags$i(class = "fa fa-circle",
                            style = "color: steelblue"),
        no = shiny::tags$i(class = "fa fa-circle-o",
                           style = "color: steelblue"))
    ),
    #
    htmltools::hr(),
    shiny::tags$h4("Pre-run info"),
    shiny::verbatimTextOutput(ns("info_text"), placeholder = TRUE),
    shiny::tags$br(),
    shiny::actionButton(ns("create_actionButton"), "Create matching cohort")

  )
}


#' matchCohorts Server Function
#'
#' @description A shiny Module server function.
#'
#' @param id A module id.
#' @param r_databaseConnection A reactive database connection object.
#'
#' @importFrom shiny moduleServer reactiveValues observe observeEvent req renderText
#' @importFrom shinyjs toggleState
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom dplyr filter pull mutate
#' @importFrom stringr str_replace str_detect
#' @importFrom CohortGenerator createCohortSubsetDefinition addCohortSubsetDefinition
#' @importFrom HadesExtras createMatchingSubset
#' @importFrom ParallelLogger logInfo
#'
#' @export
mod_matchCohorts_server <- function(id, r_databaseConnection) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      cohortDefinitionSet = NULL
    )

    r_cohortDefinitionSetToAdd <- shiny::reactiveValues(
      cohortDefinitionSet = NULL
    )


    #
    # update selectCaseCohort_pickerInput with cohort names in r_databaseConnection$cohortTableHandler
    #
    shiny::observe({
      shiny::req(r_databaseConnection$cohortTableHandler)
      shiny::req(r_databaseConnection$hasChangeCounter)

      cohortIdAndNames <- r_databaseConnection$cohortTableHandler$getCohortIdAndNames()
      cohortIdAndNamesList <- list()
      if(nrow(cohortIdAndNames) != 0){
        cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, paste(cohortIdAndNames$shortName, "("  , cohortIdAndNames$cohortName, ")")))
      }

      shinyWidgets::updatePickerInput(
        inputId = "selectCaseCohort_pickerInput",
        choices = cohortIdAndNamesList,
        selected = character(0)
      )
    })


    #
    # update matchToCohortId_pickerInput with cohort names not in selectCaseCohort_pickerInput
    #
    shiny::observe({
      shiny::req(r_databaseConnection$cohortTableHandler)
      shiny::req(r_databaseConnection$hasChangeCounter)
      shiny::req(input$selectCaseCohort_pickerInput)

      cohortIdAndNames <- r_databaseConnection$cohortTableHandler$getCohortIdAndNames()|>
          dplyr::filter(!(cohortId %in% input$selectCaseCohort_pickerInput))
      cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, paste(cohortIdAndNames$shortName, "("  , cohortIdAndNames$cohortName, ")")))

      shinyWidgets::updatePickerInput(
        inputId = "selectControlCohort_pickerInput",
        choices = cohortIdAndNamesList,
        selected = character(0)
      )
    })

    #
    # adjust match ratio based on the number of cases and controls
    #
    shiny::observe({
      shiny::req(input$selectCaseCohort_pickerInput)
      shiny::req(input$selectControlCohort_pickerInput)

      cohortCounts <-  r_databaseConnection$cohortTableHandler$getCohortCounts()

      nSubjectsCase <- cohortCounts |>
        dplyr::filter(cohortId == input$selectCaseCohort_pickerInput) |>
        dplyr::pull(cohortSubjects)
      nSubjectsControl <- cohortCounts |>
        dplyr::filter(cohortId == input$selectControlCohort_pickerInput) |>
        dplyr::pull(cohortSubjects)


      estimated_matchRatio <- .estimate_matching_ratio(n_cases=nSubjectsCase, n_controls=nSubjectsControl,expected_match_fraction = 0.8)
      shiny::updateNumericInput(session, "matchRatio_numericInput", value = estimated_matchRatio)
    })

    #
    # activate settings if cohors have been selected
    #
    shiny::observe({
      condition <- !is.null(input$selectControlCohort_pickerInput)
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
      shiny::req(input$selectCaseCohort_pickerInput)
      shiny::req(input$selectControlCohort_pickerInput)


      existingSubsetDefinitionIds <- r_databaseConnection$cohortTableHandler$cohortDefinitionSet |>
        dplyr::filter(!is.na(subsetDefinitionId)) |>
        dplyr::pull(subsetDefinitionId)

      nextSubsetDefinitionId <- ifelse( length(existingSubsetDefinitionIds) == 0, 1, max(existingSubsetDefinitionIds)+1 )


      # Match to sex and bday, match ratio 10
      subsetDef <- CohortGenerator::createCohortSubsetDefinition(
        name = "",
        definitionId = nextSubsetDefinitionId,
        subsetOperators = list(
          HadesExtras::createMatchingSubset(
            matchToCohortId = input$selectCaseCohort_pickerInput,
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
        cohortDefinitionSet = r_databaseConnection$cohortTableHandler$cohortDefinitionSet,
        cohortSubsetDefintion = subsetDef,
        targetCohortIds = as.integer(input$selectControlCohort_pickerInput),
        overwriteExisting =  TRUE
      )

      cohortDefinitionSetOnlyNew <- cohortDefinitionSet |>
        dplyr::filter(subsetDefinitionId == nextSubsetDefinitionId)

      # update cohortId to non existing, to avoid overflow here
      # https://github.com/OHDSI/CohortGenerator/blob/e3efad630b8b2c0376431a88fde89e6c4bbac38c/R/SubsetDefinitions.R#L193
      previousCohortId <- cohortDefinitionSetOnlyNew$cohortId
      unusedCohortId <- setdiff(1:1000, r_databaseConnection$cohortTableHandler$cohortDefinitionSet |> dplyr::pull(cohortId)) |> head(1)

      cohortDefinitionSetOnlyNew <- cohortDefinitionSetOnlyNew |>
        dplyr::mutate(
          cohortId = unusedCohortId,
          sql = stringr::str_replace(sql, paste0('cohort_definition_id = ', previousCohortId), paste0('cohort_definition_id = ', unusedCohortId)),
          sql = stringr::str_replace(sql, paste0(previousCohortId, ' as cohort_definition_id'), paste0(unusedCohortId, ' as cohort_definition_id'))
        )

      r$cohortDefinitionSet <- cohortDefinitionSetOnlyNew

    })

    #
    # Render temporal name
    #
    output$info_text <- shiny::renderText({

      if (!shiny::isTruthy(r$cohortDefinitionSet) || !shiny::isTruthy(input$selectCaseCohort_pickerInput) || !shiny::isTruthy(input$selectControlCohort_pickerInput)) {
        return("")
      }

      cohortsOverlap <- r_databaseConnection$cohortTableHandler$getCohortsOverlap()
      cohortCounts <-  r_databaseConnection$cohortTableHandler$getCohortCounts()

      nSubjectsOverlap <- cohortsOverlap |>
        dplyr::filter(
          stringr::str_detect(cohortIdCombinations, paste0("-", input$selectCaseCohort_pickerInput, "-")) &
          stringr::str_detect(cohortIdCombinations, paste0("-",input$selectControlCohort_pickerInput, "-"))
        ) |>
        dplyr::pull(numberOfSubjects)  |>
        sum()
      nSubjectsCase <- cohortCounts |>
        dplyr::filter(cohortId == input$selectCaseCohort_pickerInput) |>
        dplyr::pull(cohortSubjects)
      nSubjectsControl <- cohortCounts |>
        dplyr::filter(cohortId == input$selectControlCohort_pickerInput) |>
        dplyr::pull(cohortSubjects)

      # name
      message <- paste0("\u2139\uFE0F New cohort name : ", r$cohortDefinitionSet$cohortName, " \n")

      # counts
      if( nSubjectsCase > nSubjectsControl ){
        message <- paste0(message, "\u274C There are more subjects in matching/case cohort (", nSubjectsCase,") than in target/control cohort (", nSubjectsControl,"). Are you sure they are correct?\n")
      }else{
        if( nSubjectsCase * input$matchRatio_numericInput > nSubjectsControl){
          message <- paste0(message, "\u26A0\uFE0F There may be few subjects in target/control cohort (", nSubjectsControl,") to match from (number of subjects in matching/case * matching ratio = ",nSubjectsCase * input$matchRatio_numericInput,")\n")
        }
      }

      # overlap
      if(nSubjectsOverlap==0){
        message <- paste0(message, "\u2705 No subjects overlap between matching/case and target/control \n")
      }else{
        if(nSubjectsOverlap > nSubjectsCase * .20){
          message <- paste0(message, "\u274C There are many subjects, ",nSubjectsOverlap, ", that overlap between matching/case and target/control cohorts. Consider removing them in Operate Cohorts tab\n")
        }else{
          message <- paste0(message, "\u26A0\uFE0F There are few subjects, ",nSubjectsOverlap, ", that overlap between matching/case and target/control cohorts. \n")
        }
      }



      ParallelLogger::logInfo("[Match cohorts] Match Settings: ", message)

      return(message)
    })


    #
    # click to build cohort
    #
    shiny::observeEvent(input$create_actionButton, {
      shiny::req(r$cohortDefinitionSet)

      ## copy selected to
      r_cohortDefinitionSetToAdd$cohortDefinitionSet <-  r$cohortDefinitionSet

      ParallelLogger::logInfo("[Match cohorts] Creating cohorts: ", r_cohortDefinitionSetToAdd$cohortDefinitionSet$cohortName,
                              " with ids: ", r_cohortDefinitionSetToAdd$cohortDefinitionSet$cohortId,
                              " to database", input$selectDatabases_pickerInput)

    })

    #
    # evaluate the cohorts to append; if accepted increase output to trigger closing actions
    #
    rf_append_accepted_counter <- mod_fct_appendCohort_server("import_atlas", r_databaseConnection, r_cohortDefinitionSetToAdd )

    # close and reset
    shiny::observeEvent(rf_append_accepted_counter(), {
      # change in r_workbench$cohortsSummaryDatabases will update output$selectDatabases_pickerInput_uiOutput <- shiny::renderUI({
      # this will chain update the rest
      r$cohortDefinitionSet <- NULL
    })




  })
}


.estimate_matching_ratio <- function(n_cases, n_controls,
                                    expected_match_fraction = 0.8,
                                    min_controls_per_case = 1) {
  # Estimate number of usable, good-quality controls
  # expected_match_fraction should ideally be obtained empirically from our data
  usable_controls <- floor(n_controls * expected_match_fraction)
  ratio <- floor(usable_controls / n_cases)
  ratio <- max(min_controls_per_case, ratio)
  return(ratio)
}













