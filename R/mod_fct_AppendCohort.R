
mod_fct_appendCohort_ui <- function() {
  shinyWidgets::useSweetAlert()
}


mod_fct_appendCohort_server <- function(id, r_connectionHandler, r_cohortDefinitionSetToAdd ){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      replaceQuestion = NULL,
      appendAcceptedCounter = 0
    )

    #
    # if r_appendCohort is modified
    #
    shiny::observeEvent(r_cohortDefinitionSetToAdd$cohortDefinitionSet, {
      shiny::req(r_cohortDefinitionSetToAdd$cohortDefinitionSet)

      # ask if existing cohorts should be replaced
      namesExistInWorkbech <- intersect(
        r_connectionHandler$cohortTableHandler$getCohortIdAndNames() |> dplyr::pull(cohortName),
        r_cohortDefinitionSetToAdd$cohortDefinitionSet |> dplyr::pull(cohortName)
      )

      if (length(namesExistInWorkbech) > 0) {
        shinyWidgets::confirmSweetAlert(
          session = session,
          inputId = "replaceQuestion_alert",
          type = "question",
          title = "Some selected cohorts had been alredy imported:",
          text = htmltools::HTML(
            "The following cohorts had been alredy imported : <ul>",
            paste0(paste0("<li> ", namesExistInWorkbech, "</li>"), collapse = "\n"),
            "</ul> Should these be replaced or ignored."
          ),
          btn_labels = c("Not-import", "Replace"),
          html = TRUE
        )
      }else{
        r$replaceQuestion <- TRUE
      }
    })
    ## just pass the info to make it writable
    shiny::observe({
      r$replaceQuestion <- input$replaceQuestion_alert
    })

    #
    # confirmSweetAlert replaceQuestion_alert
    #
    shiny::observeEvent(r$replaceQuestion, {
      shiny::req(r$replaceQuestion)
      shiny::req(r_cohortDefinitionSetToAdd$cohortDefinitionSet)

      sweetAlert_spinner("Processing cohorts")
      ParallelLogger::logInfo("[Import] Cohorts Replace", r$replaceQuestion)

      if(r$replaceQuestion){

        cohortDefinitionSet <- r_cohortDefinitionSetToAdd$cohortDefinitionSet
        # TEMP FIX this should be moved to HadesExtras::correctCohortDefinitioSet
        if (!("subsetDefinitionId" %in% names(cohortDefinitionSet))) {
          cohortDefinitionSet <- cohortDefinitionSet |> dplyr::mutate(subsetDefinitionId = cohortId)
        }
        # TEMP FIX
        cohortDefinitionSet <- cohortDefinitionSet |>
          dplyr::left_join(
            r_connectionHandler$cohortTableHandler$getCohortIdAndNames() |>
              dplyr::rename(existingCohortId = cohortId, existingSubsetDefinitionId = subsetDefinitionId),
            by = "cohortName"
          ) |>
          dplyr::mutate(
            cohortId = dplyr::if_else(!is.na(existingCohortId), existingCohortId, cohortId),
            subsetDefinitionId = dplyr::if_else(!is.na(existingSubsetDefinitionId), existingSubsetDefinitionId, subsetDefinitionId)
          ) |>
          dplyr::select(-existingCohortId, -existingSubsetDefinitionId)

        r_connectionHandler$cohortTableHandler$insertOrUpdateCohorts(cohortDefinitionSet)
        r_connectionHandler$hasChangeCounter <- r_connectionHandler$hasChangeCounter + 1
      }

      # reset module
      r$replaceQuestion <- NULL

      # pass action
      r$appendAcceptedCounter <- r$appendAcceptedCounter+1

      remove_sweetAlert_spinner()
    })


    return(shiny::reactive(r$appendAcceptedCounter))
  })
}
