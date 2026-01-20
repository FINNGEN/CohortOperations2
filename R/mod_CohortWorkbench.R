#' Cohort Workbench UI Module
#'
#' This module provides the UI for the Cohort Workbench.
#'
#' @param id A unique identifier for the module.
#'
#' @return A UI definition for the Cohort Workbench.
#'
#' @importFrom shiny NS
#' @importFrom htmltools tagList
#' @importFrom shinyWidgets useSweetAlert
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyFeedback useShinyFeedback
#' @importFrom reactable reactableOutput
#'
#' @export
mod_cohortWorkbench_ui <- function(id){
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyWidgets::useSweetAlert(),
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),

    htmltools::tags$style(HTML("
      .foldable-summary {
        cursor: pointer;
        font-weight: bold;
        list-style: none;
        position: relative;
        padding-left: 1.2em;
      }
      .foldable-summary::-webkit-details-marker {
        display: none;
      }
      .foldable-summary::before {
        content: '\\25B6';  /* closed arrow */
        position: absolute;
        left: 0;
      }
      details[open] .foldable-summary::before {
        content: '\\25BC';  /* open arrow */
      }

      /* toolbar */
      .cw-toolbar{
        display:flex;
        justify-content:flex-end;
        align-items:center;
        gap: 0.4rem;
        flex-wrap:wrap;
        margin: 0.25rem 0 0.5rem 0;
      }

      /* Remove Shiny's default spacing */
      .cw-toolbar .form-group,
      .cw-toolbar .shiny-input-container{
        margin-bottom: 0 !important;
      }

      /* Uniform, smaller buttons */
      .cw-toolbar .btn{
        height: 30px;
        padding: 4px 10px;
        font-size: 13px;
      }

      /* File input sizing */
      .cw-file{
        width: 300px;
        max-width: 100%;
      }

      .cw-file .form-control{
        height: 30px !important;
        font-size: 13px;
      }

      .cw-file .input-group-btn .btn{
        height: 30px !important;
        padding: 4px 10px !important;
        font-size: 13px;
      }

      /* Hide upload progress bar (keeps toolbar slim) */
      .cw-file .progress{
        display: none !important;
      }

      /* Small screens: allow wrapping but keep right bias */
      @media (max-width: 700px){
        .cw-toolbar{
          justify-content:flex-start;
        }
        .cw-file{
          width: 100%;
        }
      }

      /* Toolbar background strip */
      .cw-toolbar-container{
        background-color: #f7f9fb;       /* very light grey/blue */
        border-bottom: 1px solid #e3e6ea;
        padding: 6px 10px;
        margin: -1px -1px 6px -1px;       /* align with panel edges */
      }

      /* Keep toolbar compact inside container */
      .cw-toolbar{
        margin: 0;
      }

    ")),

    # remember workbench fold or open states
    htmltools::tags$script(HTML(sprintf("
      (function() {
        const key = '%s';

        document.addEventListener('DOMContentLoaded', function () {
          const details = document.getElementById(key);
          if (!details) return;

          const saved = sessionStorage.getItem(key);

          // for initial loading, make it open. Then keep it closed when users go to other tabs
          if (saved === null) {
            details.setAttribute('open', '');
            sessionStorage.setItem(key, 'closed');
          }

          // restore previous state if open
          if (saved === 'open') {
            details.setAttribute('open', '');
          }

          // persist on toggle
          details.addEventListener('toggle', function () {
            sessionStorage.setItem(key, details.open ? 'open' : 'closed');
          });
        });
      })();
    ", ns("cw_details")))),


    htmltools::tags$details(
      id = ns("cw_details"),  # or TRUE to show it expanded by default
      htmltools::tags$summary( class = "foldable-summary", shiny::uiOutput(ns("cw_summary"))),

      # Toolbar
      htmltools::div(
        class = "cw-toolbar-container",

        htmltools::div(
          class = "cw-toolbar cw-toolbar-right",
          shiny::downloadButton(ns("cw_downloadJson"), "Save workbench", icon = shiny::icon("download")),

          htmltools::div(
            class = "cw-file",
            shiny::fileInput(ns("cw_uploadJson"), placeholder = "Upload workbench from file", label = NULL, accept = c(".json"))
          ),

          shiny::actionButton(ns("cw_importJson"), "Load workbench", icon = shiny::icon("upload")),
          shiny::actionButton(ns("cw_clearAll"), "Clean workbench", icon = shiny::icon("trash"))
        )
      ),

      # the cohorts table
      reactable::reactableOutput(ns("cohortsSummaryDatabases_reactable"))
    )

  )
}
#' Cohort Workbench Server Module
#'
#' This module provides the server logic for the Cohort Workbench.
#'
#' @param id A unique identifier for the module.
#' @param r_databaseConnection A reactiveValues object containing the database connection and handlers.
#' @param table_editing A logical value indicating whether table editing is enabled. Default is TRUE.
#'
#' @return A server module for the Cohort Workbench.
#'
#' @importFrom shiny moduleServer reactiveValues observeEvent req showModal modalDialog textInput actionButton modalButton removeModal
#' @importFrom shinyWidgets confirmSweetAlert
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback
#' @importFrom shinyjs toggleState
#' @importFrom reactable renderReactable
#' @importFrom purrr pluck
#' @importFrom dplyr pull setdiff
#'
#' @export
mod_cohortWorkbench_server <- function(id, r_databaseConnection, table_editing = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    r_tmp <- shiny::reactiveValues(
      cohortNames = NULL,
      shortNames = NULL
    )

    r_workbench <- shiny::reactiveValues(
      cohortCount = 0
    )

    r_cohortDefinitionSetToAdd_forImportingWorkbench <- reactiveValues(cohortDefinitionSet = NULL)

    #
    # Renders cohortsSummaryDatabases_reactable
    #
    output$cohortsSummaryDatabases_reactable <- reactable::renderReactable({
      shiny::req(r_databaseConnection$cohortTableHandler)
      shiny::req(r_databaseConnection$hasChangeCounter)

      r_databaseConnection$cohortTableHandler$getCohortsSummary() |>
        HadesExtras::rectable_cohortsSummary(
          deleteButtonsShinyId = ns("cohortsWorkbenchDeleteButtons"),
          editButtonsShinyId = ns("cohortsWorkbenchEditButtons"))
    })

    #
    # Inform the number of cohorts in the workbench
    #

    shiny::observe({
      shiny::req(r_databaseConnection$cohortTableHandler)
      shiny::req(r_databaseConnection$hasChangeCounter)

      r_workbench$cohortCount <- nrow(r_databaseConnection$cohortTableHandler$getCohortsSummary())
    })

    output$cw_summary <- shiny::renderUI({
      n <- r_workbench$cohortCount

      if (n == 0) {
        htmltools::HTML("Show / Hide Cohort Workbench (Empty)")
      } else if (n == 1) {
        htmltools::HTML("Show / Hide Cohort Workbench (1 cohort)")
      } else {
        htmltools::HTML(
          paste0("Show / Hide Cohort Workbench (", n, " cohorts)")
        )
      }
    })

    #
    # Ask for confirmation when delete button is clicked
    #
    shiny::observeEvent(input$cohortsWorkbenchDeleteButtons, {

      cohortsSummary <- r_databaseConnection$cohortTableHandler$getCohortsSummary()
      rowNumber <- input$cohortsWorkbenchDeleteButtons$index

      cohortName <- cohortsSummary |> purrr::pluck("cohortName", rowNumber)
      shortName <- cohortsSummary |> purrr::pluck("shortName", rowNumber)

      shinyWidgets::confirmSweetAlert(
        session = session,
        inputId = ns("confirmSweetAlert_CohortsWorkbenchDeleteButtons"),
        type = "question",
        title = "Delete cohort ?",
        text = htmltools::HTML(paste0(
          "Are you sure you want to delete cohort<br>", shortName,": '", cohortName, "' ?"
        )),
        btn_labels = c("Cancel", "Delete"),
        html = TRUE
      )

    })

    #
    # If delete confirmation accepted, deletes cohort and updates r_workbench
    #
    shiny::observeEvent(input$confirmSweetAlert_CohortsWorkbenchDeleteButtons, {
      if (input$confirmSweetAlert_CohortsWorkbenchDeleteButtons == TRUE) {

        cohortsSummary <- r_databaseConnection$cohortTableHandler$getCohortsSummary()
        rowNumber <- input$cohortsWorkbenchDeleteButtons$index

        databaseName <- cohortsSummary |> purrr::pluck("databaseName", rowNumber)
        cohortId <- cohortsSummary |> purrr::pluck("cohortId", rowNumber)

        r_databaseConnection$cohortTableHandler$deleteCohorts(as.integer(cohortId))
        r_databaseConnection$hasChangeCounter = r_databaseConnection$hasChangeCounter + 1

      }
    })


    #
    # Show modal when edit button is clicked
    #
    shiny::observeEvent(input$cohortsWorkbenchEditButtons, {

      cohortsSummary <- r_databaseConnection$cohortTableHandler$getCohortsSummary()
      rowNumber <- input$cohortsWorkbenchEditButtons$index

      cohortName <- cohortsSummary |> purrr::pluck("cohortName", rowNumber)
      shortName <- cohortsSummary |> purrr::pluck("shortName", rowNumber)

      r_tmp$cohortNames <- cohortsSummary |> dplyr::pull("cohortName") |> setdiff(cohortName)
      r_tmp$shortNames <- cohortsSummary |> dplyr::pull("shortName") |> setdiff(shortName)

      shiny::showModal(
        shiny::modalDialog(
          shiny::tags$h4("Edit cohort ", shortName, ":", cohortName),
          shiny::textInput(ns("editShortName_textInput"), "Short name", value = shortName),
          shiny::textInput(ns("editCohortName_textInput"), "Cohort name", value = cohortName),
          shiny::actionButton(ns("editCohort_actionButton"), "Save"),
          shiny::modalButton("Cancel"),
          footer = NULL
        )
      )
    })

    #
    # shinyFeedback on the edit cohort modal
    #
    shiny::observeEvent(c(input$editShortName_textInput,input$editCohortName_textInput), {

      activeAcceptButton <- TRUE

      if (input$editShortName_textInput %in% r_tmp$shortNames) {
        shinyFeedback::showFeedbackDanger( inputId = "editShortName_textInput", text = "Short name exists" )
        activeAcceptButton <- FALSE
      }else if (nchar(input$editShortName_textInput) > 10) {
        shinyFeedback::showFeedbackDanger( inputId = "editShortName_textInput", text = "Short name cannot be more than 10 characters" )
        activeAcceptButton <- FALSE
      }else{
        shinyFeedback::hideFeedback(inputId =  "editShortName_textInput")
      }

      if (input$editCohortName_textInput %in% r_tmp$cohortNames) {
        shinyFeedback::showFeedbackDanger( inputId = "editCohortName_textInput", text = "Cohort name exists" )
        activeAcceptButton <- FALSE
      }else if (nchar(input$editShortName_textInput) > 500) {
        shinyFeedback::showFeedbackDanger( inputId = "editCohortName_textInput", text = "Short name cannot be more than 500 characters" )
        activeAcceptButton <- FALSE
      }else{
        shinyFeedback::hideFeedback(inputId =  "editCohortName_textInput")
      }

      shinyjs::toggleState("editCohort_actionButton", condition = activeAcceptButton )
    })

    #
    # Save cohort when editCohort_actionButton is clicked
    #
    shiny::observeEvent(input$editCohort_actionButton, {

      cohortsSummary <- r_databaseConnection$cohortTableHandler$getCohortsSummary()
      rowNumber <- input$cohortsWorkbenchEditButtons$index

      cohortId <- cohortsSummary |> purrr::pluck("cohortId", rowNumber)

      newShortName <- input$editShortName_textInput
      newCohortName <- input$editCohortName_textInput

      r_databaseConnection$cohortTableHandler$updateCohortNames(cohortId, newCohortName, newShortName)
      r_databaseConnection$hasChangeCounter = r_databaseConnection$hasChangeCounter + 1

      shiny::removeModal()
    })

    #
    # clean, save and load the entire cohort workbench
    #

    shiny::observeEvent(input$cw_clearAll, {
      shiny::req(r_databaseConnection$cohortTableHandler)

      cohortsSummary <- r_databaseConnection$cohortTableHandler$getCohortsSummary()
      n <- nrow(cohortsSummary)

      shinyWidgets::confirmSweetAlert(
        session = session,
        inputId = ns("confirmSweetAlert_ClearAll"),
        type = "warning",
        title = "Clean cohort workbench?",
        text = htmltools::HTML(paste0(
          "This will delete <b>all</b> cohorts currently in the workbench (", n, ").<br>",
          "This action cannot be undone."
        )),
        btn_labels = c("Cancel", "Delete all"),
        html = TRUE
      )
    })

    shiny::observeEvent(input$confirmSweetAlert_ClearAll, {
      if (isTRUE(input$confirmSweetAlert_ClearAll)) {

        cohortsSummary <- r_databaseConnection$cohortTableHandler$getCohortsSummary()
        cohortIds <- cohortsSummary$cohortId

        if (length(cohortIds) > 0) {
          r_databaseConnection$cohortTableHandler$deleteCohorts(as.integer(cohortIds))
          r_databaseConnection$hasChangeCounter <- r_databaseConnection$hasChangeCounter + 1
        }
      }
    })

    # save workbench
    output$cw_downloadJson <- shiny::downloadHandler(
      filename = function() {
        paste0("CO_cohort_workbench_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
      },
      content = function(file) {
        shiny::req(r_databaseConnection$cohortTableHandler)
        shiny::req(r_databaseConnection$cohortTableHandler$cohortDefinitionSet)

        cds <- r_databaseConnection$cohortTableHandler$cohortDefinitionSet

        cds_df <- as.data.frame(cds, stringsAsFactors = FALSE)

        payload <- list(
          schema_version = 1,
          exported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
          cohortDefinitionSet = cds_df
        )

        jsonlite::write_json(payload, path = file, auto_unbox = TRUE, pretty = TRUE, null = "null")
      }
    )


    # import work bench

    shiny::observeEvent(input$cw_importJson, {
      shiny::req(input$cw_uploadJson)

      shinyWidgets::confirmSweetAlert(
        session = session,
        inputId = ns("confirmSweetAlert_ImportJson"),
        type = "question",
        title = "Load all cohorts in imported file?",
        text = "This will import cohorts from the selected JSON file into the Cohort Workbench.",
        btn_labels = c("Cancel", "Import")
      )
    })

    shiny::observeEvent(input$confirmSweetAlert_ImportJson, {
      if (!isTRUE(input$confirmSweetAlert_ImportJson)) return()

      shiny::req(r_databaseConnection$cohortTableHandler)
      shiny::req(input$cw_uploadJson)

      path <- input$cw_uploadJson$datapath
      payload <- jsonlite::read_json(path, simplifyVector = TRUE)

      cds_in <- NULL
      if (!is.null(payload$cohortDefinitionSet)) {
        cds_in <- payload$cohortDefinitionSet
      } else if (is.data.frame(payload)) {
        # allow raw df-json as fallback
        cds_in <- payload
      } else {
        shiny::showNotification("Invalid JSON: expected 'cohortDefinitionSet'.", type = "error")
        return()
      }

      if (!is.data.frame(cds_in)) {
        shiny::showNotification(
          "Invalid JSON: cohortDefinitionSet must be a data.frame-like structure.",
          type = "error"
        )
        return()
      }

      # shortName is optional
      required_cols <- c("cohortId", "cohortName", "sql", "json")
      missing_cols <- setdiff(required_cols, names(cds_in))
      if (length(missing_cols) > 0) {
        shiny::showNotification(
          paste0("Invalid JSON: missing columns: ", paste(missing_cols, collapse = ", ")),
          type = "error"
        )
        return()
      }

      cds_in$cohortId <- as.integer(cds_in$cohortId)

      if (!("subsetDefinitionId" %in% names(cds_in))) cds_in$subsetDefinitionId <- cds_in$cohortId
      if (!("subsetParent" %in% names(cds_in))) cds_in$subsetParent <- cds_in$cohortId
      if (!("isSubset" %in% names(cds_in))) cds_in$isSubset <- FALSE

      # If present, coerce to expected types
      if ("subsetParent" %in% names(cds_in)) cds_in$subsetParent <- as.integer(cds_in$subsetParent)
      if ("subsetDefinitionId" %in% names(cds_in)) cds_in$subsetDefinitionId <- as.integer(cds_in$subsetDefinitionId)
      if ("isSubset" %in% names(cds_in)) cds_in$isSubset <- as.logical(cds_in$isSubset)
      if ("shortName" %in% names(cds_in)) cds_in$shortName <- as.character(cds_in$shortName)

      cds_tbl <- tibble::as_tibble(cds_in)

      # render sql for importing operated cohorts (not needed for base or main cohorts)
      handler <- r_databaseConnection$cohortTableHandler

      needs_render <- cds_tbl$isSubset %in% TRUE

      if (any(needs_render, na.rm = TRUE)) {

        render_one_sql <- function(sql) {
          if (is.na(sql) || !nzchar(sql)) return(sql)

          # only render if placeholders are present
          if (!grepl("@[A-Za-z0-9_]+", sql)) return(sql)

          SqlRender::render(
            sql,
            cohort_database_schema = handler$cohortDatabaseSchema,
            cohort_table = handler$cohortTableNames$cohortTable,
            target_database_schema = handler$cohortDatabaseSchema,
            target_cohort_table = handler$cohortTableNames$cohortTable
          )
        }

        cds_tbl$sql[needs_render] <- vapply(cds_tbl$sql[needs_render], render_one_sql, character(1))
      }

      r_import$lastImportedN <- nrow(cds_tbl)

      r_cohortDefinitionSetToAdd_forImportingWorkbench$cohortDefinitionSet <- cds_tbl
    })

     rf_workbench_import_counter <- mod_fct_appendCohort_server(
      "workbench_import_file",
      r_databaseConnection,
      r_cohortDefinitionSetToAdd_forImportingWorkbench
    )

    # reactive values for exporting and importing the workbench
    r_import <- shiny::reactiveValues(
      lastImportedN = NULL,
      lastCounter = 0
    )

    shiny::observeEvent(rf_workbench_import_counter(), {
      # only react on increment
      shiny::req(!is.null(rf_workbench_import_counter()))
      if (rf_workbench_import_counter() <= r_import$lastCounter) return()

      r_import$lastCounter <- rf_workbench_import_counter()

      n <- r_import$lastImportedN
      if (is.null(n)) {
        shiny::showNotification("Cohort import succeeded.", type = "message")
      } else {
        shiny::showNotification(paste0("Imported ", n, " cohorts."), type = "message")
      }
      # reset
      r_import$lastImportedN <- NULL
    })

    .has_method <- function(obj, name) {
      !is.null(obj) && (name %in% names(obj)) && is.function(obj[[name]])
    }





  })
}
