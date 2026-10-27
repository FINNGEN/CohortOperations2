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
    shiny::tags$h5("Create a new cohort by selecting comparable participants from a target/control cohort."),
    .co2_help(
      "Help with cohort matching",
      shiny::p("Select the cohort to match (usually the cases) and the cohort from which to select people (usually the controls). The ratio is the maximum number of eligible controls selected for each case. It can be lower if too few controls meet the selected criteria."),
      shiny::p("Same Sex and Same Birth Year retain only controls with the same recorded sex or birth year as the case they are paired with. Start date within target's/control's observation period retains only controls whose own cohort period covers the matched case's cohort start date: control start date ≤ case start date ≤ control end date."),
      shiny::p("The date choices determine the dates stored in the new matched-control cohort. Keep retains the selected control's own start or end date. Match copies the corresponding date from the case it was matched to. Check Pre-run info before creating the cohort.")
    ),
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
    shiny::helpText(shiny::textOutput(ns("matchingRatioSuggestion_textOutput"))),
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
      label = "Matched control must be observed at case cohort entry date (index date)",
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
      cohortDefinitionSet = NULL,
      matchingCohortName = NULL,
      pendingFeasibility = NULL
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

      cohortIdAndNamesList <- list()
      if(nrow(cohortIdAndNames) != 0){
        cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, paste(cohortIdAndNames$shortName, "("  , cohortIdAndNames$cohortName, ")")))
      }

      shinyWidgets::updatePickerInput(
        inputId = "selectControlCohort_pickerInput",
        choices = cohortIdAndNamesList,
        selected = character(0)
      )
    })

    # Suggest a ratio based on 80% of the available controls when both cohorts
    # have been selected. The user can change this value before creating the
    # matching cohort.
    shiny::observeEvent(
      list(input$selectCaseCohort_pickerInput, input$selectControlCohort_pickerInput),
      {
        shiny::req(r_databaseConnection$cohortTableHandler)
        shiny::req(input$selectCaseCohort_pickerInput)
        shiny::req(input$selectControlCohort_pickerInput)

        cohort_counts <- r_databaseConnection$cohortTableHandler$getCohortCounts()
        n_subjects_case <- cohort_counts |>
          dplyr::filter(cohortId == input$selectCaseCohort_pickerInput) |>
          dplyr::pull(cohortSubjects)
        n_subjects_control <- cohort_counts |>
          dplyr::filter(cohortId == input$selectControlCohort_pickerInput) |>
          dplyr::pull(cohortSubjects)

        shiny::req(length(n_subjects_case) == 1, length(n_subjects_control) == 1)
        suggested_ratio <- .estimate_matching_ratio(
          n_cases = n_subjects_case,
          n_controls = n_subjects_control,
          expected_match_fraction = 0.8
        )

        shiny::updateNumericInput(
          session,
          "matchRatio_numericInput",
          value = suggested_ratio
        )
      },
      ignoreInit = TRUE
    )

    output$matchingRatioSuggestion_textOutput <- shiny::renderText({
      shiny::req(r_databaseConnection$cohortTableHandler)
      shiny::req(input$selectCaseCohort_pickerInput)
      shiny::req(input$selectControlCohort_pickerInput)

      cohort_counts <- r_databaseConnection$cohortTableHandler$getCohortCounts()
      n_subjects_case <- cohort_counts |>
        dplyr::filter(cohortId == input$selectCaseCohort_pickerInput) |>
        dplyr::pull(cohortSubjects)
      n_subjects_control <- cohort_counts |>
        dplyr::filter(cohortId == input$selectControlCohort_pickerInput) |>
        dplyr::pull(cohortSubjects)
      shiny::req(length(n_subjects_case) == 1, length(n_subjects_control) == 1)

      suggested_ratio <- .estimate_matching_ratio(
        n_cases = n_subjects_case,
        n_controls = n_subjects_control,
        expected_match_fraction = 0.8
      )
      paste0(
        "Suggested starting ratio: 1:", suggested_ratio,
        " (based on 80% of available controls). You can change it."
      )
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


      result <- .build_matching_cohort_definition_set(
        cohort_definition_set = r_databaseConnection$cohortTableHandler$cohortDefinitionSet,
        case_cohort_id = input$selectCaseCohort_pickerInput,
        control_cohort_id = input$selectControlCohort_pickerInput,
        match_ratio = input$matchRatio_numericInput,
        match_sex = input$matchSex_switch,
        match_birth_year = input$matchBirthYear_switch,
        match_start_date = input$matchCohortStartDateWithInDuration_switch,
        new_cohort_start_date = input$newCohortStartDate_option,
        new_cohort_end_date = input$newCohortEndDate_option
      )

      r$cohortDefinitionSet <- result$cohort_definition_set
      r$matchingCohortName <- result$matching_cohort_name

    })

    #
    # Render temporal name
    #
    output$info_text <- shiny::renderText({

      if (!shiny::isTruthy(r$cohortDefinitionSet) || !shiny::isTruthy(input$selectCaseCohort_pickerInput) || !shiny::isTruthy(input$selectControlCohort_pickerInput) || !shiny::isTruthy(input$matchRatio_numericInput)) {
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
      message <- paste0("\u2139\uFE0F New cohort name : ", r$matchingCohortName, " \n")

      # counts
      feasibility <- .assess_matching_feasibility(
        n_cases = nSubjectsCase,
        n_controls = nSubjectsControl,
        requested_ratio = input$matchRatio_numericInput
      )

      if (nSubjectsCase > nSubjectsControl) {
        message <- paste0(message, "\u274C There are more subjects in matching/case cohort (", nSubjectsCase,") than in target/control cohort (", nSubjectsControl,"). Are you sure they are correct?\n")
      }

      if (!feasibility$sufficient_controls) {
        message <- paste0(
          message,
          "\u26A0\uFE0F The requested 1:", feasibility$requested_ratio,
          " match requires ", feasibility$requested_controls,
          " controls, but only ", feasibility$n_controls,
          " are available. The maximum average ratio before matching criteria is approximately 1:",
          format(feasibility$available_average_ratio, digits = 3, trim = TRUE),
          ". You will be asked to confirm before the cohort is created.\n"
        )
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
    append_matching_cohort <- function() {
      ## copy selected to
      r_cohortDefinitionSetToAdd$cohortDefinitionSet <- r$cohortDefinitionSet

      ParallelLogger::logInfo("[Match cohorts] Creating cohorts: ", r_cohortDefinitionSetToAdd$cohortDefinitionSet$cohortName,
                              " with ids: ", r_cohortDefinitionSetToAdd$cohortDefinitionSet$cohortId,
                              " to database", input$selectDatabases_pickerInput)
    }

    confirm_matching_cohort <- function() {
      shiny::removeModal()
      append_matching_cohort()
    }

    show_insufficient_controls_modal <- function(feasibility) {
      r$pendingFeasibility <- feasibility

      action_buttons <- shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(ns("confirm_matching_actionButton"), "Continue with available controls", class = "btn-warning")
      )

      if (feasibility$subsample_cases_possible) {
        action_buttons <- shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("subsample_matching_actionButton"), "Randomly subsample cases", class = "btn-primary"),
          shiny::actionButton(ns("confirm_matching_actionButton"), "Continue with available controls", class = "btn-warning")
        )
      }

      option_explanation <- if (feasibility$subsample_cases_possible) {
        shiny::tags$p(
          "Choose 'Randomly subsample cases' to keep up to ", feasibility$maximum_subsampled_cases,
          " case subjects at an effective 1:", feasibility$subsample_ratio,
          " ratio and create both cohorts. Choose 'Continue with available controls' to keep all ",
          feasibility$n_cases, " case subjects and use the eligible controls available for each case. The requested ratio stays unchanged."
        )
      } else {
        shiny::tags$p(
          "The available control pool is too small to retain even one case at the requested ratio. " ,
          "Choose 'Continue with available controls' to keep all case subjects and use the eligible controls available for each case. The requested ratio stays unchanged."
        )
      }

      shiny::showModal(
        shiny::modalDialog(
          title = "Not enough controls for the requested matching ratio",
          shiny::tags$p(
            paste0(
              "The selected cohorts contain ", feasibility$n_cases, " case subjects and ",
              feasibility$n_controls, " control subjects. A 1:", feasibility$requested_ratio,
              " match would request ", feasibility$requested_controls, " controls."
            )
          ),
          shiny::tags$p(
            paste0(
              "The maximum average ratio available before applying matching criteria is approximately 1:",
              format(feasibility$available_average_ratio, digits = 3, trim = TRUE), "."
            )
          ),
          shiny::tags$p(
            "Sex, birth year, and index-date matching may reduce the number of eligible controls further."
          ),
          option_explanation,
          footer = action_buttons,
          easyClose = FALSE
        )
      )
    }

    show_subsample_confirmation_modal <- function(feasibility) {
      shiny::showModal(
        shiny::modalDialog(
          title = "Confirm random case subsampling",
          shiny::tags$p(
            paste0(
              "There are ", feasibility$n_controls, " control subjects. At 1:",
              feasibility$subsample_ratio, ", this allows up to ", feasibility$maximum_subsampled_cases,
              " of the ", feasibility$n_cases, " case subjects to be retained before matching criteria are applied."
            )
          ),
          shiny::tags$p(
            paste0(
              "CO2 will randomly select up to ", feasibility$maximum_subsampled_cases,
              " case subjects using the fixed seed ", .matching_random_seed,
              " and create the subsampled case cohort and its matched control cohort at 1:",
              feasibility$subsample_ratio, "."
            )
          ),
          shiny::tags$p(
            "Matching criteria may leave fewer eligible controls, so the final matched cohort may contain fewer controls than the requested ratio."
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(ns("confirm_subsample_actionButton"), "Create subsampled and matched cohorts", class = "btn-primary")
          ),
          easyClose = FALSE
        )
      )
    }

    shiny::observeEvent(input$create_actionButton, {
      shiny::req(r$cohortDefinitionSet)

      cohortCounts <- r_databaseConnection$cohortTableHandler$getCohortCounts()
      nSubjectsCase <- cohortCounts |>
        dplyr::filter(cohortId == input$selectCaseCohort_pickerInput) |>
        dplyr::pull(cohortSubjects)
      nSubjectsControl <- cohortCounts |>
        dplyr::filter(cohortId == input$selectControlCohort_pickerInput) |>
        dplyr::pull(cohortSubjects)

      feasibility <- .assess_matching_feasibility(
        n_cases = nSubjectsCase,
        n_controls = nSubjectsControl,
        requested_ratio = input$matchRatio_numericInput
      )

      subsampling <- .assess_matching_subsampling(
        n_cases = feasibility$n_cases,
        n_controls = feasibility$n_controls,
        requested_ratio = feasibility$requested_ratio
      )
      feasibility$subsample_ratio <- subsampling$subsample_ratio
      feasibility$maximum_subsampled_cases <- subsampling$maximum_subsampled_cases
      feasibility$subsample_cases_possible <- subsampling$subsample_cases_possible

      if (!feasibility$sufficient_controls) {
        show_insufficient_controls_modal(feasibility)
      } else {
        append_matching_cohort()
      }
    })

    shiny::observeEvent(input$confirm_matching_actionButton, {
      shiny::req(r$cohortDefinitionSet)
      confirm_matching_cohort()
    })

    shiny::observeEvent(input$subsample_matching_actionButton, {
      shiny::req(r$cohortDefinitionSet, r$pendingFeasibility)
      show_subsample_confirmation_modal(r$pendingFeasibility)
    })

    shiny::observeEvent(input$confirm_subsample_actionButton, {
      shiny::req(r$pendingFeasibility)

      result <- .build_matching_cohort_definition_set(
        cohort_definition_set = r_databaseConnection$cohortTableHandler$cohortDefinitionSet,
        case_cohort_id = input$selectCaseCohort_pickerInput,
        control_cohort_id = input$selectControlCohort_pickerInput,
        match_ratio = r$pendingFeasibility$subsample_ratio,
        match_sex = input$matchSex_switch,
        match_birth_year = input$matchBirthYear_switch,
        match_start_date = input$matchCohortStartDateWithInDuration_switch,
        new_cohort_start_date = input$newCohortStartDate_option,
        new_cohort_end_date = input$newCohortEndDate_option,
        subsample_cases = TRUE,
        subsampled_case_count = r$pendingFeasibility$maximum_subsampled_cases,
        random_seed = .matching_random_seed
      )

      r$cohortDefinitionSet <- result$cohort_definition_set
      r$matchingCohortName <- result$matching_cohort_name
      shiny::removeModal()
      append_matching_cohort()
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


.assess_matching_feasibility <- function(n_cases, n_controls, requested_ratio) {
  checkmate::assertNumeric(n_cases, len = 1, finite = TRUE, lower = 0)
  checkmate::assertNumeric(n_controls, len = 1, finite = TRUE, lower = 0)
  checkmate::assertNumeric(requested_ratio, len = 1, finite = TRUE, lower = 1)

  n_cases <- as.numeric(n_cases)
  n_controls <- as.numeric(n_controls)
  requested_ratio <- as.numeric(requested_ratio)

  requested_controls <- n_cases * requested_ratio
  available_average_ratio <- if (n_cases == 0) Inf else n_controls / n_cases

  list(
    n_cases = n_cases,
    n_controls = n_controls,
    requested_ratio = requested_ratio,
    requested_controls = requested_controls,
    available_average_ratio = available_average_ratio,
    sufficient_controls = n_controls >= requested_controls
  )
}


.assess_matching_subsampling <- function(n_cases, n_controls, requested_ratio) {
  feasibility <- .assess_matching_feasibility(
    n_cases = n_cases,
    n_controls = n_controls,
    requested_ratio = requested_ratio
  )

  subsample_ratio <- if (feasibility$n_controls < feasibility$n_cases) {
    1
  } else {
    feasibility$requested_ratio
  }
  maximum_subsampled_cases <- floor(
    feasibility$n_controls / subsample_ratio
  )

  list(
    subsample_ratio = subsample_ratio,
    maximum_subsampled_cases = maximum_subsampled_cases,
    subsample_cases_possible = feasibility$n_controls > 0 &&
      maximum_subsampled_cases >= 1 &&
      maximum_subsampled_cases < feasibility$n_cases
  )
}


# Fixed seed used for reproducible case subsampling in the matching module.
.matching_random_seed <- 20261023L


#' Build matching subset definitions
#'
#' Creates the subset definitions used by the matching module. When case
#' subsampling is requested, a deterministic random case subset is created
#' first and the matching subset references that generated cohort.
#'
#' @param cohort_definition_set Existing cohort definition set.
#' @param case_cohort_id Case cohort ID.
#' @param control_cohort_id Control cohort ID.
#' @param match_ratio Number of controls requested per case.
#' @param match_sex Whether to match on sex.
#' @param match_birth_year Whether to match on birth year.
#' @param match_start_date Whether to match the case start date within the
#'   control observation period.
#' @param new_cohort_start_date Start-date behavior for matched controls.
#' @param new_cohort_end_date End-date behavior for matched controls.
#' @param subsample_cases Whether to create a random case subset first.
#' @param subsampled_case_count Number of cases to retain when subsampling.
#' @param random_seed Seed used for deterministic case subsampling.
#'
#' @return A list containing the new cohort definition set, matching cohort
#'   name, and generated cohort IDs.
#' @noRd
.build_matching_cohort_definition_set <- function(
    cohort_definition_set,
    case_cohort_id,
    control_cohort_id,
    match_ratio,
    match_sex,
    match_birth_year,
    match_start_date,
    new_cohort_start_date,
    new_cohort_end_date,
    subsample_cases = FALSE,
    subsampled_case_count = NULL,
    random_seed = .matching_random_seed) {

  if (!"subsetDefinitionId" %in% names(cohort_definition_set)) {
    cohort_definition_set$subsetDefinitionId <- NA_integer_
  }
  if (!"shortName" %in% names(cohort_definition_set)) {
    cohort_definition_set$shortName <- NA_character_
  }

  existing_subset_definition_ids <- cohort_definition_set |>
    dplyr::filter(!is.na(subsetDefinitionId)) |>
    dplyr::pull(subsetDefinitionId)

  next_definition_id <- if (length(existing_subset_definition_ids) == 0) {
    1L
  } else {
    max(existing_subset_definition_ids) + 1L
  }

  working_definition_set <- cohort_definition_set
  definition_ids <- integer(0)
  generated_case_cohort_id <- as.integer(case_cohort_id)

  if (subsample_cases) {
    checkmate::assertIntegerish(subsampled_case_count, len = 1, lower = 1)
    checkmate::assertFALSE(anyNA(subsampled_case_count))

    case_subset_definition <- CohortGenerator::createCohortSubsetDefinition(
      name = "random_case_subsample",
      definitionId = next_definition_id,
      subsetOperators = list(
        HadesExtras::createRandomSubset(
          numberOfSubjects = subsampled_case_count,
          randomSeed = random_seed
        )
      )
    )

    working_definition_set <- CohortGenerator::addCohortSubsetDefinition(
      cohortDefinitionSet = working_definition_set,
      cohortSubsetDefintion = case_subset_definition,
      targetCohortIds = as.integer(case_cohort_id),
      overwriteExisting = TRUE
    )

    generated_case_cohort_id <- working_definition_set |>
      dplyr::filter(subsetDefinitionId == next_definition_id) |>
      dplyr::pull(cohortId)

    definition_ids <- c(definition_ids, next_definition_id)
    next_definition_id <- next_definition_id + 1L
  }

  matching_subset_definition <- CohortGenerator::createCohortSubsetDefinition(
    name = "match",
    definitionId = next_definition_id,
    subsetOperators = list(
      HadesExtras::createMatchingSubset(
        matchToCohortId = generated_case_cohort_id,
        matchRatio = match_ratio,
        matchSex = match_sex,
        matchBirthYear = match_birth_year,
        matchCohortStartDateWithInDuration = match_start_date,
        newCohortStartDate = new_cohort_start_date,
        newCohortEndDate = new_cohort_end_date
      )
    )
  )

  working_definition_set <- CohortGenerator::addCohortSubsetDefinition(
    cohortDefinitionSet = working_definition_set,
    cohortSubsetDefintion = matching_subset_definition,
    targetCohortIds = as.integer(control_cohort_id),
    overwriteExisting = TRUE
  )
  definition_ids <- c(definition_ids, next_definition_id)

  new_cohorts <- working_definition_set |>
    dplyr::filter(subsetDefinitionId %in% definition_ids)

  # Use small unused IDs in the temporary cohort definition set and update all
  # generated SQL references, including the match-to case subset ID.
  unused_cohort_ids <- setdiff(
    1:1000,
    cohort_definition_set |> dplyr::pull(cohortId)
  ) |> head(nrow(new_cohorts))

  if (length(unused_cohort_ids) != nrow(new_cohorts)) {
    stop("Unable to allocate temporary cohort IDs for the matching subsets")
  }

  old_cohort_ids <- new_cohorts$cohortId
  new_cohort_ids <- as.integer(unused_cohort_ids)

  for (index in seq_along(old_cohort_ids)) {
    new_cohorts$sql <- stringr::str_replace_all(
      new_cohorts$sql,
      paste0("(?<![0-9])", old_cohort_ids[[index]], "(?![0-9])"),
      as.character(new_cohort_ids[[index]])
    )
  }

  new_cohorts <- new_cohorts |>
    dplyr::mutate(cohortId = new_cohort_ids)

  matching_cohort <- new_cohorts |>
    dplyr::filter(subsetDefinitionId == max(definition_ids))

  if (subsample_cases) {
    case_subset_index <- new_cohorts$subsetDefinitionId == min(definition_ids)
    matching_subset_index <- new_cohorts$subsetDefinitionId == max(definition_ids)
    case_cohort_name <- cohort_definition_set |>
      dplyr::filter(cohortId == case_cohort_id) |>
      dplyr::pull(cohortName)
    control_cohort_name <- cohort_definition_set |>
      dplyr::filter(cohortId == control_cohort_id) |>
      dplyr::pull(cohortName)
    case_short_name <- cohort_definition_set |>
      dplyr::filter(cohortId == case_cohort_id) |>
      dplyr::pull(shortName)
    control_short_name <- cohort_definition_set |>
      dplyr::filter(cohortId == control_cohort_id) |>
      dplyr::pull(shortName)

    if (length(case_short_name) == 0 || is.na(case_short_name[[1]])) {
      case_short_name <- HadesExtras::makeShortName(case_cohort_name, case_cohort_id)
    } else {
      case_short_name <- case_short_name[[1]]
    }
    if (length(control_short_name) == 0 || is.na(control_short_name[[1]])) {
      control_short_name <- HadesExtras::makeShortName(control_cohort_name, control_cohort_id)
    } else {
      control_short_name <- control_short_name[[1]]
    }

    new_cohorts$cohortName[case_subset_index] <- paste0(
      case_cohort_name,
      " - random subsample (",
      subsampled_case_count,
      " subjects; seed ",
      random_seed,
      ")"
    )
    new_cohorts$cohortName[matching_subset_index] <- paste0(
      control_cohort_name,
      " - matched to random case subsample (1:",
      match_ratio,
      ")"
    )
    new_cohorts$shortName[case_subset_index] <- paste0(
      case_short_name,
      "_RS",
      subsampled_case_count
    )
    new_cohorts$shortName[matching_subset_index] <- paste0(
      control_short_name,
      "_M",
      match_ratio
    )
    matching_cohort <- new_cohorts |>
      dplyr::filter(subsetDefinitionId == max(definition_ids))
  }

  list(
    cohort_definition_set = new_cohorts,
    matching_cohort_name = matching_cohort$cohortName[[1]],
    generated_cohort_ids = new_cohort_ids
  )
}













