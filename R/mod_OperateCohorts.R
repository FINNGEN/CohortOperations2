
#' Operate Cohorts UI Module
#'
#' This module provides the UI for operating cohorts, including defining cohort operations,
#' visualizing cohort intersections with an UpSet plot, and creating new cohorts.
#'
#' @param id A namespace identifier for the module.
#'
#' @importFrom shiny NS tagList tags plotOutput textOutput actionButton
#' @importFrom htmltools hr div
#' @importFrom shinyjs useShinyjs
#'
#' @return A UI definition for the operate cohorts module.
mod_operateCohorts_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    mod_fct_appendCohort_ui(),
    shinyjs::useShinyjs(),
    shiny::tags$h5("Create a new cohort by defining an set of operations with the cohorts in the workbench."),
    htmltools::hr(),
    shiny::tags$h4("Operation - expression defining the cohort"),
    htmltools::div(style = "width: 90%; height: 260; overflow: auto; margin-left: 30px; padding: 10px;",
                   mod_dragAndDrop_ui(ns("dragAndDrop"))
    ),
    #
    shiny::tags$h4("New cohort name"),
    shiny::textOutput(ns("newCohortName_text")),
    #
    htmltools::hr(),
    shiny::tags$h4("Cohort intersections - UpSet plot"),
    htmltools::div(style = "width: 90%; height: 300; overflow: auto; margin-left: 30px; padding: 10px;",
                   shiny::plotOutput(ns("upsetPlot"), width = "80%", height = "250px"),
    ),
    htmltools::hr(),
    shiny::actionButton(ns("create_actionButton"), "Create new cohort")
  )
}
#' Operate Cohorts Server Module
#'
#' This module provides the server logic for operating cohorts, including defining cohort operations,
#' visualizing cohort intersections with an UpSet plot, and creating new cohorts.
#'
#' @param id A namespace identifier for the module.
#' @param r_databaseConnection A reactive database connection object.
#'
#' @importFrom shiny moduleServer reactiveValues observe observeEvent renderText renderPlot req isTruthy
#' @importFrom shinyjs toggleState
#' @importFrom dplyr filter pull mutate select group_by summarise across all_of any_of
#' @importFrom stringr str_extract str_replace_all str_detect str_c str_split
#' @importFrom rlang syms
#' @importFrom ggplot2 ggplot aes geom_bar guides geom_text scale_fill_grey labs theme_light theme element_text
#' @importFrom ggupset scale_x_upset
#' @importFrom forcats as_factor
#' @importFrom tidyr unnest
#' @importFrom CohortGenerator createCohortSubsetDefinition addCohortSubsetDefinition
#' @importFrom HadesExtras operationStringToSQL createOperationSubset
#' @importFrom ParallelLogger logInfo
#'
#' @return A server module for operating cohorts.
mod_operateCohorts_server <- function(id, r_databaseConnection) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      cohortDefinitionSet = NULL,
      operationStringError = NULL,
      operationString = NULL
    )

    r_cohortDefinitionSetToAdd <- shiny::reactiveValues(
      cohortDefinitionSet = NULL
    )

    #
    # get operation string from dragAndDrop module
    #
    rf_operationString <- mod_dragAndDrop_server("dragAndDrop", r_databaseConnection)
    # copy to reactive value for easier testing
    shiny::observe(
      r$operationString <- rf_operationString()
    )

    #
    # when operation string changes check if it is valid
    #
    shiny::observe({
      shiny::req(r$operationString)

      error <- NULL
      tryCatch({
        HadesExtras::operationStringToSQL(r$operationString)
      }, error = function(e){
        error <<- e$message
      })

      r$operationStringError <- error

    })


    #
    # if operation string is valid create temporal cohortDefinitionSet and render name
    #
    shiny::observe({
      shiny::req(r$operationString)
      # if not valid set cohortDefinitionSet to NULL
      if(!is.null(r$operationStringError)){
        r$cohortDefinitionSet <- NULL
      }else{
        existingSubsetDefinitionIds <- r_databaseConnection$cohortTableHandler$cohortDefinitionSet |>
          dplyr::filter(!is.na(subsetDefinitionId)) |>
          dplyr::pull(subsetDefinitionId)

        nextSubsetDefinitionId <- ifelse( length(existingSubsetDefinitionIds) == 0, 1, max(existingSubsetDefinitionIds)+1 )

        subsetDef <- CohortGenerator::createCohortSubsetDefinition(
          name = "",
          definitionId = nextSubsetDefinitionId,
          subsetOperators = list(
            HadesExtras::createOperationSubset(
              operationString = r$operationString
            )
          )
        )

        # TEMP::get first integer From operation string
        targetCohortIds <- as.integer(stringr::str_extract(r$operationString, "\\d+"))

        # two adjacent cohorts crash the following function (malformed input)
        cohortDefinitionSet <- CohortGenerator::addCohortSubsetDefinition(
          cohortDefinitionSet = r_databaseConnection$cohortTableHandler$cohortDefinitionSet,
          cohortSubsetDefintion = subsetDef,
          targetCohortIds = targetCohortIds,
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
      }
    })

    #
    # Render temporal name
    #
    output$newCohortName_text <- shiny::renderText({
      if(is.null(r$cohortDefinitionSet) && is.null(r$operationStringError)){
        stringToShow  <- "----"
      }
      if(!is.null(r$operationStringError)){
        stringToShow  <- paste("\u274C", r$operationStringError)
      }
      if(!is.null(r$cohortDefinitionSet)){
        stringToShow  <- paste("\u2705", r$cohortDefinitionSet$cohortName)
      }

      ParallelLogger::logInfo("[Operate cohorts] Operation: ", stringToShow)

      stringToShow
    })

    #
    # create upset plot
    #
    output$upsetPlot <- shiny::renderPlot({
      if(is.null(r$cohortDefinitionSet)){
        return(NULL)
      } else {
        if(shiny::isTruthy(r$cohortDefinitionSet)){

          s <- r$operationString
          cohortsInOperation <- as.character(stringr::str_extract_all(s, "\\d+")[[1]])
          cohortOverlap <- r_databaseConnection$cohortTableHandler$getCohortsOverlap()

          # TEMP FIX
          # extract all numbers from string s
          for (cohortId in cohortsInOperation) {
            cohortOverlap <- cohortOverlap |>
              dplyr::mutate(!!cohortId := stringr::str_detect(cohortIdCombinations, paste0("-", cohortId, "-")))
          }

          cohortOverlap <- cohortOverlap |>
            dplyr::select(-cohortIdCombinations)
          # END TEMP FIX

          expression <- s |>
            stringr::str_replace_all("\\d+", "`\\0`") |>
            stringr::str_replace_all("Upd", "|") |>
            stringr::str_replace_all("Mp", "&!") |>
            stringr::str_replace_all("Ip", "&")

          expressionFilter  <-  paste0('`', cohortsInOperation, '`') |>
            paste(collapse = "|")

          trycatch <- tryCatch({
            cohortOverlap <- cohortOverlap |>
              dplyr::select(-dplyr::any_of('newset')) |>
              dplyr::mutate(newset = eval(parse(text = expression))) |>
              dplyr::select(numberOfSubjects, one_of(cohortsInOperation), newset) |>
              dplyr::filter(eval(parse(text = expressionFilter)))
          }, error = function(e){
            return(NULL)
          })

          # make the rows distinct keeping the total number of subjects
          cohortOverlap <- cohortOverlap |>
            dplyr::group_by_if(is.logical) |>
            dplyr::summarise(numberOfSubjects = sum(numberOfSubjects), .groups = "drop")

          .plot_upset_cohortsOverlap(cohortOverlap, r_databaseConnection$cohortTableHandler$getCohortIdAndNames() |> dplyr::select(cohortId, shortName) )
        }
      }
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
      r_cohortDefinitionSetToAdd$databaseName <- input$selectDatabases_pickerInput
      r_cohortDefinitionSetToAdd$cohortDefinitionSet <-  r$cohortDefinitionSet

      ParallelLogger::logInfo("[Operate cohorts] Creating cohorts: ", r_cohortDefinitionSetToAdd$cohortDefinitionSet$cohortName,
                              " with ids: ", r_cohortDefinitionSetToAdd$cohortDefinitionSet$cohortId,
                              " to database ", input$selectDatabases_pickerInput)

    })

    #
    # evaluate the cohorts to append; if accepted increase output to trigger closing actions
    #
    rf_append_accepted_counter <- mod_fct_appendCohort_server("matchCohort", r_databaseConnection, r_cohortDefinitionSetToAdd )

    # close and reset
    shiny::observeEvent(rf_append_accepted_counter(), {
      # change in r_workbench$cohortsSummaryDatabases will update output$selectDatabases_pickerInput_uiOutput <- shiny::renderUI({
      r$cohortDefinitionSet <- NULL
    })
  })
}

#' Plot UpSet Cohorts Overlap
#'
#' This function generates an UpSet plot to visualize the overlap between different cohorts.
#'
#' @param cohortOverlap A data frame containing the overlap information between cohorts.
#' @param nameToShortName A data frame mapping cohort names to their short names.
#'
#' @return A ggplot object representing the UpSet plot of cohort overlaps.
#' @importFrom dplyr select mutate across all_of cur_column group_by summarise left_join
#' @importFrom stringr str_c str_split
#' @importFrom rlang syms
#' @importFrom tidyr unnest
#' @importFrom ggplot2 ggplot aes geom_bar guides geom_text scale_fill_grey labs theme_light theme element_text
#' @importFrom ggupset scale_x_upset
#' @importFrom forcats as_factor
.plot_upset_cohortsOverlap <- function(cohortOverlap, nameToShortName) {

  # check if newset column is present
  if(!"newset" %in% colnames(cohortOverlap)){
    return(NULL)
  }

  cohortNames <- cohortOverlap |> dplyr::select(-numberOfSubjects) |> colnames()  |> setdiff("newset")

  cohortOverlapPlot  <- cohortOverlap |>
    # convert true false to cohort names
    dplyr::mutate(dplyr::across(dplyr::all_of(cohortNames), ~ifelse(., as.character(dplyr::cur_column()), '')))  |>
    # join cohort names
    dplyr::mutate(cohort_vector = stringr::str_c( !!!rlang::syms(cohortNames), sep = " ")) |>
    # string to vector
    dplyr::mutate(cohort_vector = stringr::str_split(cohort_vector, " "))

  #TMP
  cohortOverlapPlot <- cohortOverlapPlot |>
    dplyr::select(newset, numberOfSubjects, cohort_vector) |>
    tidyr::unnest(cohort_vector) |>
    dplyr::left_join(nameToShortName |> dplyr::mutate(cohortId = as.character(cohortId)), by = c("cohort_vector" = "cohortId")) |>
    dplyr::select(-cohort_vector) |>
    dplyr::group_by(newset, numberOfSubjects) |>
    dplyr::summarise(cohort_vector = list(shortName))

  ###
  ## function
  ###
  g <- NULL
  try({
    g <- cohortOverlapPlot |>
      dplyr::mutate(newset = forcats::as_factor(!newset)) |>
      #
      ggplot2::ggplot(ggplot2::aes(x=cohort_vector, y=numberOfSubjects, fill=newset, label = numberOfSubjects)) +
      ggplot2::geom_bar(stat = "identity") +
      # upset x
      ggupset::scale_x_upset() +
      # style
      ggplot2::guides(fill = "none") +
      ggplot2::geom_text( nudge_y = 160, size=5) +
      ggplot2::scale_fill_grey(drop = FALSE) +
      ggplot2::labs(x = "Cohort Sets", y = "N patients")+
      ggplot2::theme_light() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 15),
        axis.text.x = ggplot2::element_text(size = 15),
        axis.text.y = ggplot2::element_text(size = 15)
      )
  })

  return(g)
}
