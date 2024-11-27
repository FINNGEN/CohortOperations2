#
# import
#
import_ui <- function(id) {
  ns <- shiny::NS(id)
  from <- c("file", "copypaste")
  file_extensions <- c(".tsv", ".csv", ".txt", ".xls", ".xlsx")

  file <- shiny::tabPanelBody(
    value = "file",
    shiny::tags$br(),
    datamods::import_file_ui(id = ns("file"), title = NULL, file_extensions = file_extensions, preview_data = FALSE)
  )

  copypaste <- shiny::tabPanelBody(
    value = "copypaste",
    shiny::tags$br(),
    datamods::import_copypaste_ui(id = ns("copypaste"), title = NULL, name_field = FALSE)
  )

  labsImport <- list(
    "file" = "External file",
    "copypaste" = "Copy / Paste"
  )
  iconsImport <- list(
    "file" = phosphoricons::ph("file-arrow-down", title = labsImport$file),
    "copypaste" = phosphoricons::ph("clipboard-text", title = labsImport$copypaste)
  )

  tabsetPanelArgs <- (list(
    file, copypaste,
    id = ns("tabs-import"),
    type = "hidden"
  ))

  importTab <- do.call(
    what = tabsetPanel,
    args = tabsetPanelArgs
  )

  importTab <- shiny::fluidRow(
    shiny::column(
      width = 3,
      shiny::tags$br(),
      shiny::tags$style(
        shiny::HTML(sprintf("#%s>.btn-group-vertical {width: 100%%;}", ns("from"))),
        shiny::HTML(sprintf(".btn-group-vertical>.btn-group>.btn {text-align: left;}"))
      ),
      shinyWidgets::radioGroupButtons(
        inputId = ns("from"),
        label = ("How to import data?"),
        choiceValues = from,
        choiceNames = lapply(
          X = from,
          FUN = function(x) {
            tagList(iconsImport[[x]], labsImport[[x]])
          }
        ),
        direction = "vertical",
        width = "100%"
      )
    ),
    shiny::column(
      width = 9,
      importTab
    ),
    actionButton(
      inputId = ns("confirm_import"),
      label = tagList(
        phosphoricons::ph("arrow-circle-right", title = ("Import data")),
        ("Import data")
      ),
      width = "95%",
      disabled = "disabled",
      class = "btn-primary text-center",
      style = "display: block; margin: 0 auto;",
      `aria-label` = ("Import data")
    )
  )

  tags$div(
    class = "datamods-imports",
    tags$style(".tui-grid-cell-summary {vertical-align: baseline;}"),
    importTab
  )
}

import_server <- function(id, r_importedData) {
  shiny::moduleServer(id, function(input, output, session) {
    rf_importedTableFile <- datamods::import_file_server("file", trigger_return = "change", btn_show_data = FALSE)
    rf_importedTableCopypaste <- datamods::import_copypaste_server("copypaste", trigger_return = "change", btn_show_data = FALSE)

    # Add observer to update selected tab
    shiny::observeEvent(input$from, {
      shiny::updateTabsetPanel(session, "tabs-import", selected = input$from)
    })

    shiny::observeEvent(input$confirm_import, {
      if (input$from == "file") {
        r_importedData$data <- rf_importedTableFile$data()
        r_importedData$name <- rf_importedTableFile$name()
      } else if (input$from == "copypaste") {
        r_importedData$data <- rf_importedTableCopypaste$data()
        r_importedData$name <- rf_importedTableCopypaste$name()
      }
      shiny::removeModal()
    })

    shiny::observeEvent(input$close_import_modal, {
      r_importedData$data <- NULL
      r_importedData$name <- NULL
      shiny::removeModal()
    })
  })
}

import_modal <- function(id) {
  title <- ("Import data")
  size <- "l"

  button_close <- htmltools::tags$button(
    phosphoricons::ph("x", title = ("Close"), height = "2em"),
    class = "btn btn-link",
    style = htmltools::css(border = "0 none", position = "absolute", top = "5px", right = "5px"),
    `data-dismiss` = "modal",
    `data-bs-dismiss` = "modal",
    `aria-label` = ("Close"),
    onclick = "Shiny.setInputValue('close_import_modal', Math.random())" # Add onclick event
  )

  showModal(modalDialog(
    title = tagList(
      button_close,
      title
    ),
    import_ui(id),
    size = size,
    footer = NULL
  ))
}


#
# column mapping
#
mapping_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    # Add shinyfeedback dependency
    shinyFeedback::useShinyFeedback(),
    #
    shiny::tags$p(
      shiny::HTML(
        "Columns in the imported data could not be mapped automatically. This dialog allows you to manually map the columns to cohortData format.<br>",
        "The expected cohortData format is:",
        "<ul>",
        "<li>cohort_name <character>: groups persons into cohorts, if not provided only one cohort will be created with the name as the file name</li>",
        "<li>person_source_value <character>: person identifier</li>",
        "<li>cohort_start_date <date>: start date of the cohort, if not provided the cohort start date will be the date of first observation</li>",
        "<li>cohort_end_date <date>: end date of the cohort, if not provided the cohort end date will be the date of last observation</li>",
        "</ul>",
        "<b>Imported data preview</b> shows the first 5 rows of the imported data.<br>",
        "<b>Mapping menu</b> allows you to select the columns to map to the cohortData format and provide a name for the cohort.<br>"
      )
    ),
    shiny::hr(),
    shiny::tags$h4("Imported data preview (first 5 rows)"),
    reactable::reactableOutput(outputId = ns("input_table")),
    shiny::hr(),
    shiny::tags$h4("Mapping menu"),
    shiny::fluidRow(
      shiny::column(
        3,
        shinyWidgets::pickerInput(
          inputId = ns("cohort_name"),
          label = "Cohort Name",
          choices = NULL
        )
      ),
      shiny::column(
        3,
        shinyWidgets::pickerInput(
          inputId = ns("person_id"),
          label = "Person ID",
          choices = NULL
        )
      ),
      shiny::column(
        3,
        shinyWidgets::pickerInput(
          inputId = ns("start_date"),
          label = "Cohort Start Date",
          choices = NULL
        )
      ),
      shiny::column(
        3,
        shinyWidgets::pickerInput(
          inputId = ns("end_date"),
          label = "Cohort End Date",
          choices = NULL
        )
      )
    ),
    shiny::hr(),
    shiny::actionButton(
      inputId = ns("confirm"),
      label = tagList(
        phosphoricons::ph("arrow-circle-right", title = ("Import data")),
        ("Import data")
      ),
      width = "100%",
      disabled = FALSE,
      class = "btn-primary",
      `aria-label` = ("Import data")
    )
  )
}

mapping_server <- function(id, r_dataToMap, r_cohortData, regex_person_id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Summary table
    output$input_table <- reactable::renderReactable({
      shiny::req(r_dataToMap$data)
      reactable::reactable(
        data = utils::head(r_dataToMap$data, 5),
        striped = TRUE,
        defaultPageSize = 5
      )
    })

    # Update picker inputs with column names
    shiny::observe({
      if (is.null(r_dataToMap$data)) {
        shinyWidgets::updatePickerInput(
          inputId = "cohort_name",
          choices = NULL
        )
        shinyWidgets::updatePickerInput(
          inputId = "person_id",
          choices = NULL
        )
        shinyWidgets::updatePickerInput(
          inputId = "start_date",
          choices = NULL
        )
        shinyWidgets::updatePickerInput(
          inputId = "end_date",
          choices = NULL
        )
        return()
      }
      cols <- r_dataToMap$data |>
        base::names()

      possibleColPersonIds <- tibble::tibble(
        cols = cols,
        data = r_dataToMap$data[1, ] |> base::as.character()
      ) |>
        dplyr::filter(
          stringr::str_detect(data, regex_person_id())
        ) |>
        dplyr::pull(cols)

      shinyWidgets::updatePickerInput(
        inputId = "cohort_name",
        choices = c("Default to file name", cols),
        selected = "Default to file name",
        choicesOpt = list(
          style = ifelse(c(TRUE, rep(FALSE, length(cols))),
            "color: rgba(119, 119, 119, 0.5);",
            ""
          )
        )
      )
      shinyWidgets::updatePickerInput(
        inputId = "person_id",
        choices = cols,
        selected = possibleColPersonIds[1]
      )
      shinyWidgets::updatePickerInput(
        inputId = "start_date",
        choices = c("Fill with first observation date", cols),
        selected = "Fill with first observation date",
        choicesOpt = list(
          style = ifelse(c(TRUE, rep(FALSE, length(cols))),
            "color: rgba(119, 119, 119, 0.5);",
            ""
          )
        )
      )
      shinyWidgets::updatePickerInput(
        inputId = "end_date",
        choices = c("Fill with last observation date", cols),
        selected = "Fill with last observation date",
        choicesOpt = list(
          style = ifelse(c(TRUE, rep(FALSE, length(cols))),
            "color: rgba(119, 119, 119, 0.5);",
            ""
          )
        )
      )
    })

    #
    # Add feedback observers
    #

    # Cohort name
    shiny::observe({
      shiny::req(r_dataToMap$data)
      shiny::req(input$cohort_name)
      # Default to file name
      if (input$cohort_name == "Default to file name") {
        shinyFeedback::hideFeedback("cohort_name")
        shinyFeedback::showFeedback(
          inputId = "cohort_name",
          text = paste("Only one cohort will be created, with name as file name: ", r_dataToMap$name),
          color = "black"
        )
        return()
      }
      # Check number of distinct values
      n_distinct <- r_dataToMap$data[[input$cohort_name]] |>
        unique() |>
        length()
      if (n_distinct > 10) {
        shinyFeedback::hideFeedback("cohort_name")
        shinyFeedback::showFeedbackWarning(
          inputId = "cohort_name",
          text = paste("This will create", n_distinct, "cohorts, are you sure is the correct column?"),
          icon = NULL
        )
        return()
      }
      # Check empty rows
      nMissing <- r_dataToMap$data |>
        dplyr::filter(is.na(!!rlang::sym(input$cohort_name))) |>
        nrow()
      if (nMissing > 0) {
        shinyFeedback::hideFeedback("cohort_name")
        shinyFeedback::showFeedbackWarning(
          inputId = "cohort_name",
          text = paste0(nMissing, " rows have no cohort name, they will be ignored"),
          icon = NULL
        )
        return()
      }
      shinyFeedback::hideFeedback("cohort_name")
    })

    # Person ID
    shiny::observe({
      shiny::req(r_dataToMap$data)
      shiny::req(input$person_id)

      if (input$person_id %in% names(r_dataToMap$data)) {
        hasPersonIdFormat <- r_dataToMap$data |>
          utils::head() |>
          dplyr::select(input$person_id) |>
          dplyr::pull(1) |>
          stringr::str_detect(regex_person_id()) |>
          base::all()

        if (!hasPersonIdFormat) {
          shinyFeedback::showFeedbackWarning(
            inputId = "person_id",
            text = "Selected column does not have the expected person ID format",
            icon = NULL
          )
          return()
        }
        nMissing <- r_dataToMap$data |>
          dplyr::filter(is.na(!!rlang::sym(input$person_id))) |>
          nrow()
        if (nMissing > 0) {
          shinyFeedback::showFeedbackWarning(
            inputId = "person_id",
            text = paste0(nMissing, " rows have missing person ID, they will be ignored"),
            icon = NULL
          )
          return()
        }
      }
      shinyFeedback::hideFeedback("person_id")
    })


    # Cohort start date
    shiny::observe({
      shiny::req(r_dataToMap$data)
      shiny::req(input$end_date, input$start_date)

      # Cohort start date
      if (input$start_date == "Fill with first observation date") {
        shinyFeedback::hideFeedback("start_date")
        shinyFeedback::showFeedback(
          inputId = "start_date",
          text = "Cohort start date will be the date of first observation",
          color = "black"
        )
      } else if (!lubridate::is.Date(r_dataToMap$data[[input$start_date]])) {
        shinyFeedback::hideFeedback("start_date")
        shinyFeedback::showFeedbackWarning(
          inputId = "start_date",
          text = "Selected column is not a date, parsing will be attempted",
          icon = NULL
        )
      } else {
        shinyFeedback::hideFeedback("start_date")
      }

      # Cohort end date
      if (input$end_date == "Fill with last observation date") {
        shinyFeedback::hideFeedback("end_date")
        shinyFeedback::showFeedback(
          inputId = "end_date",
          text = "Cohort end date will be the date of last observation",
          color = "black"
        )
      } else if (!lubridate::is.Date(r_dataToMap$data[[input$end_date]])) {
        shinyFeedback::hideFeedback("end_date")
        shinyFeedback::showFeedbackWarning(
          inputId = "end_date",
          text = "Selected column is not a date, parsing will be attempted",
          icon = NULL
        )
      } else {
        shinyFeedback::hideFeedback("end_date")
      }

      # Cohort end date > start date
      if (lubridate::is.Date(r_dataToMap$data[[input$start_date]]) & lubridate::is.Date(r_dataToMap$data[[input$end_date]])) {
        nStartBeforeEnd <- r_dataToMap$data |>
          dplyr::select(input$start_date, input$end_date) |>
          dplyr::filter(
            !!rlang::sym(input$start_date) > !!rlang::sym(input$end_date)
          ) |>
          nrow()
        if (nStartBeforeEnd > 0) {
          shinyFeedback::showFeedbackDanger(
            inputId = "start_date",
            text = paste0(nStartBeforeEnd, " rows have start date before end date"),
            icon = NULL
          )
          shinyFeedback::showFeedbackDanger(
            inputId = "end_date",
            text = paste0(nStartBeforeEnd, " rows have start date before end date"),
            icon = NULL
          )
        }
      }
    })



    #
    # Confirm button
    #

    shiny::observeEvent(input$confirm, {
      shiny::req(r_dataToMap$data)
      shiny::req(input$cohort_name, input$person_id, input$start_date, input$end_date)
      dataLength <- r_dataToMap$data |> nrow()

      # Cohort name
      if (input$cohort_name == "Default to file name") {
        cohort_name <- tibble::tibble(
          cohort_name = r_dataToMap$name |> rep(dataLength)
        )
      } else {
        cohort_name <- r_dataToMap$data |>
          dplyr::select(input$cohort_name) |>
          dplyr::rename(cohort_name = 1) |>
          dplyr::mutate(cohort_name = as.character(cohort_name)) |>
          dplyr::mutate(cohort_name = ifelse(is.na(cohort_name), "", cohort_name))
      }

      # Person ID
      person_id <- r_dataToMap$data |>
        dplyr::select(input$person_id) |>
        dplyr::rename(person_source_value = 1) |>
        dplyr::mutate(person_source_value = as.character(person_source_value)) |>
        dplyr::mutate(person_source_value = ifelse(is.na(person_source_value), "", person_source_value))
      # Cohort start date
      if (input$start_date == "Fill with first observation date") {
        cohort_start_date <- tibble::tibble(
          cohort_start_date = as.Date(NA) |> rep(dataLength)
        )
      } else {
        cohort_start_date <- r_dataToMap$data |>
          dplyr::select(input$start_date) |>
          dplyr::rename(cohort_start_date = 1) |>
          dplyr::mutate(cohort_start_date = suppressWarnings(lubridate::as_date(cohort_start_date)))
      }

      # Cohort end date
      if (input$end_date == "Fill with last observation date") {
        cohort_end_date <- tibble::tibble(
          cohort_end_date = as.Date(NA) |> rep(dataLength)
        )
      } else {
        cohort_end_date <- r_dataToMap$data |>
          dplyr::select(input$end_date) |>
          dplyr::rename(cohort_end_date = 1) |>
          dplyr::mutate(cohort_end_date = suppressWarnings(lubridate::as_date(cohort_end_date)))
      }

      r_cohortData$data <- dplyr::bind_cols(
        cohort_name,
        person_id,
        cohort_start_date,
        cohort_end_date
      )
      r_dataToMap$data <- NULL
      r_dataToMap$name <- NULL

      shiny::removeModal()
    })

    # Then we would need to match that name in the observeEvent:
    shiny::observeEvent(input$close_modal, {
      r_cohortData$data <- NULL
      r_dataToMap$data <- NULL
      r_dataToMap$name <- NULL
      shiny::removeModal()
    })
  })
}

mapping_modal <- function(id) {
  title <- ("Map colums")
  size <- "l"

  button_close <- htmltools::tags$button(
    phosphoricons::ph("x", title = ("Close"), height = "2em"),
    class = "btn btn-link",
    style = htmltools::css(border = "0 none", position = "absolute", top = "5px", right = "5px"),
    `data-dismiss` = "modal",
    `data-bs-dismiss` = "modal",
    `aria-label` = ("Close"),
    onclick = "Shiny.setInputValue('close_modal', Math.random())" # Add onclick event
  )

  showModal(modalDialog(
    title = tagList(
      button_close,
      title
    ),
    mapping_ui(id),
    size = size,
    footer = NULL
  ))
}
