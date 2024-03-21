
mod_importCohortsFromFile_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    mod_appendCohort_ui(),
    shinyjs::useShinyjs(),
    #
    shinyWidgets::pickerInput(
      inputId = ns("selectDatabases_pickerInput"),
      label = "Load patients into database:",
      choices = NULL,
      selected = NULL,
      multiple = FALSE),
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
    shiny::observe({
      # r_connectionHandlers$databasesHandlers

      databaseIdNamesList <- fct_getDatabaseIdNamesListFromDatabasesHandlers(r_connectionHandlers$databasesHandlers)

      shinyWidgets::updatePickerInput(
        inputId = "selectDatabases_pickerInput",
        choices = databaseIdNamesList,
        selected = databaseIdNamesList[1])
    })

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

      r$original_colnames <- colnames(cohortData)
      r$cohortDataUploaded <- cohortData

      if(!all(c("cohort_name", "person_source_value", "cohort_start_date", "cohort_end_date") %in% colnames(cohortData))){
          showModal(assignmentDialog())
      } else {
        # we have the correct column names
        r$cohortDataUploaded <- dplyr::select(cohortData, "cohort_name", "person_source_value", "cohort_start_date", "cohort_end_date")
        r$columnNamesOK <- TRUE
      }
    })

    #
    # helper function for the assignment dialog
    #
    assignmentDialog <- function(failed = FALSE, message = ""){
      modalDialog(
        title = "Data is not in 'cohortData' format - assign columns to correct names",
        fluidRow(column(6, div(style = "margin-left:20px;",
                               shinyWidgets::pickerInput(
                                 inputId = ns("cohort_name"),
                                 label = "Column for cohort name",
                                 choices = c("", r$original_colnames),
                                 selected = ifelse("cohort_name" %in% r$original_colnames, "cohort_name", ""),
                                 inline = FALSE,
                                 width = '300px'
                               ))),
                 column(4, offset = 0, div(style = "margin-left:10px;",
                                           shiny::textInput(
                                             inputId = ns("default_cohort_name"),
                                             label = "Cohort name (single cohort)",
                                             width = "300px",
                                             value = isolate(input$default_cohort_name)
                                           ))),
        ),
        fluidRow(column(6, div(style = "margin-left:20px;",
                               shinyWidgets::pickerInput(
                                 inputId = ns("person_source_value"),
                                 label = "Column for person identifier",
                                 choices = c("", r$original_colnames),
                                 selected = ifelse("finngenid" %in% r$original_colnames, "finngenid",
                                                   ifelse("person_source_value" %in% r$original_colnames, "person_source_value", "")),
                                 inline = FALSE,
                                 width = '300px'
                               )))
        ),
        fluidRow(column(6, div(style = "margin-left:20px;",
                               shinyWidgets::pickerInput(
                                 inputId = ns("cohort_start_date"),
                                 label = "Column for cohort start date",
                                 choices = c("", r$original_colnames),
                                 selected = ifelse("cohort_start_date" %in% r$original_colnames, "cohort_start_date", ""),
                                 inline = FALSE,
                                 width = '300px'
                               )))
        ),
        fluidRow(column(6, div(style = "margin-left:20px;",
                               shinyWidgets::pickerInput(
                                 inputId = ns("cohort_end_date"),
                                 label = "Column for cohort end date",
                                 choices = c("", r$original_colnames),
                                 selected = ifelse("cohort_end_date" %in% r$original_colnames, "cohort_end_date", ""),
                                 inline = FALSE,
                                 width = '300px'
                               )))
        ),
        shiny::actionButton(ns("import_help_button"), "Help"),
        shinyjs::hidden(
          shiny::verbatimTextOutput(ns("import_help"))
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ok"), "OK")
        ),
        size = "l",
        if (failed)
          div(tags$br(), tags$b(message, style = "color: red;"), tags$br()),
      )
    }

    #
    # check the current assignment, revisit if needed
    #
    observeEvent(input$ok, {
      removeModal()

      cohort_name <- isolate(input$cohort_name)
      default_cohort_name <- isolate(input$default_cohort_name)
      person_source_value <- isolate(input$person_source_value)
      cohort_start_date <- isolate(input$cohort_start_date)
      cohort_end_date <- isolate(input$cohort_end_date)

      all_names <- c(cohort_name, default_cohort_name, person_source_value, cohort_start_date, cohort_end_date)
      all_names <- all_names[all_names != ""]

      cohortData <- r$cohortDataUploaded

      # check if there are duplicate assignments
      if(length(all_names) != length(unique(all_names))){
        showModal(assignmentDialog(failed = TRUE, "You have assigned the same name to multiple columns"))
        return()
      }
      # cohort name is required
      if(cohort_name == "" & default_cohort_name == ""){
        showModal(assignmentDialog(failed = TRUE, "You must assign a cohort name, either a column or a default one"))
        return()
      }
      if(cohort_name != "" & default_cohort_name != ""){
        showModal(assignmentDialog(failed = TRUE, "You can give either a cohort column or a default name, not both"))
        return()
      }
      if(cohort_name == "" & default_cohort_name != ""){
        cohort_name <- "cohort_name"
        cohortData$cohort_name <- default_cohort_name
      }

      # is everything defined
      if((cohort_name == "" & default_cohort_name == "") | person_source_value == ""){
        showModal(assignmentDialog(failed = TRUE, "You must assign at least a person identifier and a cohort name"))
        return()
      }

      # check if cohort_start_date and cohort_end_date are empty, replace them with NA
      if(cohort_start_date == ""){
        cohort_start_date <- "cohort_start_date"
        cohortData$cohort_start_date <- as.Date(NA)
      }
      if(cohort_end_date == ""){
        cohort_end_date <- "cohort_end_date"
        cohortData$cohort_end_date <- as.Date(NA)
      }

      # change the column names
      cohortData <- cohortData |>
        dplyr::select(dplyr::sym(cohort_name), dplyr::sym(person_source_value), dplyr::sym(cohort_start_date), dplyr::sym(cohort_end_date))
      names(cohortData) <- c("cohort_name", "person_source_value", "cohort_start_date", "cohort_end_date")

      # force date columns to be Dates
      cohortData$cohort_start_date <- as.Date(cohortData$cohort_start_date)
      cohortData$cohort_end_date <- as.Date(cohortData$cohort_end_date)

      # we are done
      r$cohortDataUploaded <- cohortData
      r$columnNamesOK <- TRUE
    })

    #
    # check the cohortData
    #
    shiny::observe({
      shiny::req(r$columnNamesOK)
      shiny::req(r$cohortDataUploaded)

      cohortData <- r$cohortDataUploaded

      r$columnNamesOK <- FALSE

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

    output$import_help <- shiny::renderText({
      paste(
        "Cohort Operations expects the file in 'cohortData' format.",
        "There should be column names 'cohort_name', 'person_source_value', 'cohort_start_date', 'cohort_end_date',",
        "but there can be others as well.",
        "This dialog will help you to assign the columns to the correct names (does not change your file).",
        "You can select one of the columns to be the cohort name, or give a default name.",
        "The cohort start and end dates can be left empty, ",
        "then they will be set to the minimum and maximum dates in the cohort.",
        sep = "\n"
      )
    })

    # reactive function to get selected values
    r_selectedIndex <- shiny::reactive(reactable::getReactableState("cohorts_reactable", "selected", session))

    #
    # button import selected: checks selected cohorts
    #
    shiny::observe({
      shinyjs::toggleState("import_actionButton", condition = !is.null(r_selectedIndex()) )
    })

    shiny::observeEvent(input$import_help_button, {
      shinyjs::toggle("import_help")
    })

    shiny::observeEvent(input$import_actionButton, {
      shiny::req(r_selectedIndex())
      shiny::req(r$cohortData)
      shiny::req(input$selectDatabases_pickerInput)

      sweetAlert_spinner("Importing cohorts")

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

      remove_sweetAlert_spinner()

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


















