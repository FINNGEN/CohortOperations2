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
    shiny::tags$h4("Operation - expression defining the cohort"),
    #shiny::textInput(inputId = ns("operationString_textInput"), label = "Operation String"),
    htmltools::div(style = "width: 90%; height: 260; overflow: auto; margin-left: 30px; padding: 10px;",
      mod_dragAndDrop_ui(ns("dragAndDrop"))
    ),
    #
    htmltools::hr(),
    shiny::tags$h4("Cohort intersections - UpSet plot"),
    htmltools::div(style = "width: 90%; height: 300; overflow: auto; margin-left: 30px; padding: 10px;",
      shiny::plotOutput(ns("upsetPlot"), width = "80%", height = "250px"),
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

      # two adjacent cohorts crash the following function (malformed input)
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
      stringToShow <- "----"
      if(!is.null(r$operationStringError)){
        stringToShow <- paste("❌", r$operationStringError)
      }else{
        if(shiny::isTruthy(r$cohortDefinitionSet)){
          stringToShow <- paste("✅", r$cohortDefinitionSet$cohortName)
        }
      }

      ParallelLogger::logInfo("[Operate cohorts] Operation: ", stringToShow)

      stringToShow
    })

    #
    # create upset plot
    #
    output$upsetPlot <- shiny::renderPlot({

      if(!is.null(r$operationStringError)){
        return(NULL)
      } else {
        if(shiny::isTruthy(r$cohortDefinitionSet)){

          s <- rf_operationString()
          cohortsInOperation <- as.character(stringr::str_extract_all(s, "\\d+")[[1]])
          cohortTableHandler <- r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandler
          cohortOverlap <- cohortTableHandler$getCohortsOverlap()

          # TEMP FIX
          cohortOverlap <- cohortOverlap|>
            dplyr::mutate(cohortIdCombinations = paste0('-', cohortIdCombinations, '-'))

          cohortIds <- cohortOverlap |> dplyr::pull(cohortIdCombinations)  |>
            stringr::str_split("-") |> unlist() |> unique() |>
            as.numeric() |> sort() |> as.character()

          for (cohortId in cohortIds) {
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

          trycatch <- tryCatch({
            cohortOverlap <- cohortOverlap |>
              dplyr::select(-dplyr::any_of('newset')) |>
              dplyr::mutate(newset = eval(parse(text = expression))) |>
              dplyr::select(numberOfSubjects, one_of(cohortsInOperation), newset)
          }, error = function(e){
            return(NULL)
          })

          # make the rows distinct keeping the total number of subjects
          cohortOverlap <- cohortOverlap |>
            dplyr::group_by_if(is.logical) |>
            dplyr::summarise(numberOfSubjects = sum(numberOfSubjects), .groups = "drop")

          .plot_upset_cohortsOverlap(cohortOverlap)
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
      r_toAdd$databaseName <- input$selectDatabases_pickerInput
      r_toAdd$cohortDefinitionSet <-  r$cohortDefinitionSet

      ParallelLogger::logInfo("[Operate cohorts] Creating cohorts: ", r_toAdd$cohortDefinitionSet$cohortName,
                              " with ids: ", r_toAdd$cohortDefinitionSet$cohortId,
                              " to database ", input$selectDatabases_pickerInput)

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

.plot_upset_cohortsOverlap <- function(cohortOverlap) {

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
      ggplot2::geom_text( nudge_y = 60,) +
      ggplot2::scale_fill_grey(drop = FALSE) +
      ggplot2::labs(x = "Cohort Sets", y = "N patients")+
      ggplot2::theme_light() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 15)
      )
  })

  return(g)
}







