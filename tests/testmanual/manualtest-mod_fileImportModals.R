#
# import
#

devtools::load_all()

ui <- shiny::fluidPage(
  shiny::tags$h3("Import Data Example"),

  # Add button to launch modal
  shiny::actionButton("launch_modal", "Import Data"),

  # Output area for imported data
  shiny::verbatimTextOutput(outputId = "data")
)

  server <- function(input, output, session) {
    r_importedData <- shiny::reactiveValues(
      data = NULL,
      name = NULL
    )
    # Create modal when button is clickedkja
    shiny::observeEvent(input$launch_modal, {
      import_modal(id = "import_modal")
    })
    
    import_server("import_modal", r_importedData)

    # Display the imported data
    output$data <- shiny::renderPrint({
      shiny::req(r_importedData$data)
      r_importedData$data
    })
  }

  shiny::shinyApp(ui, server)


#
# mapping
#

imported_data <- tibble::tribble(
  ~cohort_name2, ~person_source_value2, ~cohort_start_date2, ~cohort_end_date2, ~other_col,
  "Cohort A", "000728a7-80de-420a-9286-2c20e81cb7b8", "2020-01-01", "2020-01-03", "A",
  "Cohort A", "000cb58f-523d-49a2-a05e-de1e93f35c01", "2020-01-01", "2020-01-03", "B", 
  "Cohort A", "001f4a87-70d0-435c-a4b9-1425f6928d33", "2020-01-01", "2020-01-03", "C",
  "Cohort A", "002805e7-7624-4cb7-b68d-e8ac92f61ff9", "2020-01-01", "2020-01-03", "D",
  "Cohort A", "0030eb48-316c-4250-907f-a272909ff8b9", "2020-01-01", "2020-01-03", "E",
  "Cohort B", "00093765-abef-4a56-9280-8f92422afae7", "2020-01-01", "2020-01-04", "F",
  "Cohort B", "00196a95-1567-41f8-b608-18e6295b4c1e", "2020-01-01", "2020-01-04", "G",
  "Cohort B", "00211cd2-f171-45d9-a7c6-ea8ac6d28d09", "2020-01-01", "2020-01-04", "H",
  "Cohort B", "0029df47-1263-4576-8ca3-4615adb7dd7a", "2020-01-01", "2020-01-04", "I",
  "Cohort B", "00444703-f2c9-45c9-a247-f6317a43a929", "2020-01-01", "2020-01-04", "J",
  "Cohort B", "00444703-f2c9-45c9-a247-f6317a43a929", "2020-01-01", "2020-01-04", "K"
) |> 
  dplyr::mutate(
    cohort_start_date2 = lubridate::as_date(cohort_start_date2),
    cohort_end_date2 = lubridate::as_date(cohort_end_date2)
  )


regex_person_id <- shiny::reactive("^00[0-9a-f]{6}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$")

devtools::load_all()

ui <- shiny::fluidPage(
  shiny::tags$h3("Mapping Data Example"),

  # Add button to launch modal
  shiny::actionButton("launch_modal", "Mapping Data"),

  # Output area for imported data
  shiny::verbatimTextOutput(outputId = "data")
)

  server <- function(input, output, session) {
    
    r_dataToMap <- reactiveValues(
      data = NULL,
      name = NULL
    )
    r_cohortData <- reactiveValues(
      data = NULL
    )

    observeEvent(input$launch_modal, {
      mapping_modal(id = "mapping_modal")
      r_dataToMap$data <- NULL
      r_dataToMap$name <- NULL
      r_dataToMap$data <- imported_data
      r_dataToMap$name <-  'file_name'
    })

    mapped <- mapping_server("mapping_modal", r_dataToMap, r_cohortData, regex_person_id)

    # Display the imported data
    output$data <- shiny::renderPrint({
      shiny::req(r_cohortData$data)
      r_cohortData$data
    })
  }


  app  <- shiny::shinyApp(ui, server)
  app



