
mod_importCohortsFromFile_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    mod_appendCohort_ui(),
    shinyjs::useShinyjs(),
    #
    shiny::uiOutput(ns("selectDatabases_pickerInput_uiOutput")),
    shiny::fileInput(ns("uploadedFile"), "Choose a file in cohortData format:",
                     multiple = FALSE,
                     accept = c("text/tsv", "text/tabular-separated-values,text/plain", ".tsv",
                                "text/csv", "text/comma-separated-values,text/plain", ".csv")
    ),
    htmltools::hr(),
    reactable::reactableOutput(ns("cohorts_reactable")), # %>% ui_load_spiner(),
    htmltools::hr(),
    shiny::actionButton(ns("import_actionButton"), "Import Selected")
  )
}

mod_importCohortsFromFile_server <- function(id, r_connectionHandlers, r_workbench) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      uploadedFile = NULL,
      cohortDataUploaded = NULL,
      cohortData = NULL,
      original_colnames = NULL,
      assigned_colnames = NULL,
      columnNamesOK = FALSE
    )

    r_toAdd <- shiny::reactiveValues(
      databaseName = NULL,
      cohortDefinitionSet = NULL
    )

    #
    # just pass the info to make it writable
    #
    shiny::observe({
      r$uploadedFile <- input$uploadedFile
    })

    #
    # render selectDatabases_pickerInput
    #
    output$selectDatabases_pickerInput_uiOutput <- shiny::renderUI({
      databaseIdNamesList <- fct_getDatabaseIdNamesListFromDatabasesHandlers(r_connectionHandlers$databasesHandlers)

      shinyWidgets::pickerInput(
        inputId = ns("selectDatabases_pickerInput"),
        label = "Load patients into databases:",
        choices = databaseIdNamesList,
        selected = databaseIdNamesList[1],
        multiple = FALSE)
    })

    #
    # keep the current selection in a reactive
    #
    observe({
      # shiny::req(input$cohort_name)
      shiny::req(input$person_source_value)
      shiny::req(input$cohort_start_date)
      shiny::req(input$cohort_end_date)

      r$assigned_colnames <- c(input$cohort_name, input$person_source_value, input$cohort_start_date, input$cohort_end_date)
    })

    #
    # check the current assignment, revisit if needed
    #
    observeEvent(input$ok, {
      removeModal()
      # error checking before removing
      if(is.null(r$assigned_colnames)){
        showModal(assignmentDialog(failed = TRUE, "All columns must be assigned!"))
        return()
      }
      if(length(unique(r$assigned_colnames)) != 4){
        showModal(assignmentDialog(failed = TRUE, "Column assignments must be unique!"))
        return()
      }
      r$columnNamesOK <- TRUE
    })

    #
    # helper function for the assignment dialog
    #
    assignmentDialog <- function(failed = FALSE, message = ""){
      modalDialog(
        title = "Assign column names",
        fluidRow(column(8, div(style = "margin-left:20px;",
                                shinyWidgets::pickerInput(
                                  inputId = ns("cohort_name"),
                                  label = "Column for cohort name",
                                  choices = r$original_colnames,
                                  selected = isolate(input$cohort_name),
                                  inline = FALSE,
                                  width = 'auto'
                                )))
        ),
        fluidRow(column(8, div(style = "margin-left:20px;",
                                shinyWidgets::pickerInput(
                                  inputId = ns("person_source_value"),
                                  label = "Column for person identifier",
                                  choices = r$original_colnames,
                                  selected = ifelse("finngenid" %in% r$original_colnames, "finngenid", ""),
                                  inline = FALSE,
                                  width = 'auto'
                                )))
        ),
        fluidRow(column(8, div(style = "margin-left:20px;",
                                shinyWidgets::pickerInput(
                                  inputId = ns("cohort_start_date"),
                                  label = "Column for cohort start date",
                                  choices = r$original_colnames,
                                  selected = isolate(input$cohort_start_date),
                                  inline = FALSE,
                                  width = 'auto'
                                )))
        ),
        fluidRow(column(8, div(style = "margin-left:20px;",
                                shinyWidgets::pickerInput(
                                  inputId = ns("cohort_end_date"),
                                  label = "Column for cohort end date",
                                  choices = r$original_colnames,
                                  selected = isolate(input$cohort_end_date),
                                  inline = FALSE,
                                  width = 'auto'
                                )))
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ok"), "OK")
        ),
        if (failed)
          div(tags$b(message, style = "color: red;")),
      )
    }

    #
    # updates r$cohortDefinitionSetImported with uploaded file, or with error
    #
    shiny::observe({
      shiny::req(r$uploadedFile)
      shiny::req(input$selectDatabases_pickerInput)

      ext <- tools::file_ext(r$uploadedFile$datapath)

      # passing error to shiny::validate
      if(ext != "tsv" & ext != "csv"){
        r$cohortDefinitionSetImported <- "ERROR READING FILE:\nI need to know if the file is in .tsv or .csv format, please set the extension accordingly"
        return()
      }

      if(ext == "tsv"){ cohortData <- HadesExtras::readCohortData(r$uploadedFile$datapath, delim = "\t") }
      if(ext == "csv"){ cohortData <- HadesExtras::readCohortData(r$uploadedFile$datapath, delim = ",") }

      # we expect lowercase names
      colnames(cohortData) <- stringr::str_to_lower(colnames(cohortData))

      r$original_colnames <- c("", colnames(cohortData))
      r$cohortDataUploaded   <- cohortData

      if(length(colnames(cohortData)) < 4){
        showModal(assignmentDialog(failed = TRUE, "There must be at least 4 columns!"))
      } else if(!all(c("cohort_name", "person_source_value", "cohort_start_date", "cohort_end_date") %in% colnames(cohortData))){
        showModal(assignmentDialog())
      } else {
        r$columnNamesOK <- TRUE
      }
    })

    #
    # rename the columns
    #
    shiny::observe({
      shiny::req(r$columnNamesOK)
      shiny::req(r$cohortDataUploaded)

      if(is.null(r$assigned_colnames)){
        cohortData <- r$cohortDataUploaded
      } else {
        required_colnames <- c("cohort_name", "person_source_value", "cohort_start_date", "cohort_end_date")
        cohortData <- r$cohortDataUploaded |> dplyr::rename_with(~ required_colnames, all_of(r$assigned_colnames))
      }

      r$columnNamesOK <- FALSE
      r$assigned_colnames <- NULL

      isCohortData <- HadesExtras::checkCohortData(cohortData)

      # passing error to shiny::validate
      if(is.character(isCohortData)){
        r$cohortData <- paste(c("ERROR READING COHORTDATA FILE:", isCohortData), sep = "\n")
      }else{
        r$cohortData <- cohortData
      }
    })

    #
    # updates output$cohorts_reactable with r$cohortDefinitionSetImported
    #
    output$cohorts_reactable <- reactable::renderReactable({
      shiny::req(r$cohortData)
      shiny::req(input$selectDatabases_pickerInput)

      shiny::validate(
        shiny::need(!is.character(r$cohortData), r$cohortData)
      )

      .reactatable_cohortData(r$cohortData)

    })

    # reactive function to get selected values
    r_selectedIndex <- shiny::reactive(reactable::getReactableState("cohorts_reactable", "selected", session))

    #
    # button import selected: checks selected cohorts
    #
    shiny::observe({
      shinyjs::toggleState("import_actionButton", condition = !is.null(r_selectedIndex()) )
    })

    shiny::observeEvent(input$import_actionButton, {
      shiny::req(r_selectedIndex())
      shiny::req(r$cohortData)
      shiny::req(input$selectDatabases_pickerInput)

      selectedCohortNames <- r$cohortData |>
        dplyr::distinct(cohort_name) |>
        dplyr::arrange(cohort_name) |>
        dplyr::slice(r_selectedIndex()) |>
        dplyr::pull(cohort_name)

      selectedCohortData <- r$cohortData |>
        dplyr::filter(cohort_name %in% selectedCohortNames)


      cohortTableHandler <- r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandler

      cohortIds <- cohortTableHandler$getCohortIdAndNames() |>
        dplyr::filter(cohortId < 1000) |> # remove ids created by subsets
        dplyr::pull(cohortId)
      cohortIdOffset <- ifelse(length(cohortIds)==0, 0L, max(cohortIds))


      ## copy selected to
      r_toAdd$databaseName <- input$selectDatabases_pickerInput
      r_toAdd$cohortDefinitionSet <-  HadesExtras::cohortDataToCohortDefinitionSet(
        cohortData = selectedCohortData,
        cohortIdOffset = cohortIdOffset,
        skipCohortDataCheck = TRUE
      )

    })

    #
    # evaluate the cohorts to append; if accepted increase output to trigger closing actions
    #
    r_append_accepted_counter <- mod_appendCohort_server("impor_file", r_connectionHandlers, r_workbench, r_toAdd )

    # close and reset
    shiny::observeEvent(r_append_accepted_counter(), {
      shinyjs::reset("uploadedFile")
      r$uploadedFile <- NULL
      r$cohortDataUploaded <- NULL
      r$cohortData <- NULL
      r_toAdd$cohortDefinitionSet <- NULL
      reactable::updateReactable("cohorts_reactable", selected = NA, session = session )
    })

  })


}



.reactatable_cohortData <- function(cohortData) {
  table <- cohortData |>
    dplyr::group_by(cohort_name) |>
    dplyr::summarise(
      n_subjects = length(unique(person_source_value)),
      n_entries = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      n_str = paste0(n_subjects, " (", n_entries, ")")
    ) |>
    dplyr::arrange(cohort_name)|>
    dplyr::select(cohort_name, n_str) |>
    #
    reactable::reactable(
      columns = list(
        cohort_name = reactable::colDef(
          name = "Cohort Name"
        ),
        n_str = reactable::colDef(
          name = "N Subjects (N Entries)"
        )
      ),
      #
      selection = "multiple",
      onClick = "select"
    )

  return(table)

}


















