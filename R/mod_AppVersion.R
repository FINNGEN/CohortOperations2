#' @title mod_appVersion_ui
#'
#' @description UI module for selecting and connecting to databases.
#'
#' @param id Shiny module ID.
#'
#' @return Shiny UI elements for the database selection module.
mod_appVersion_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tabsetPanel(
      shiny::tabPanel(
        title = "NEWS: CohortOperations2",
        htmltools::includeMarkdown(system.file("NEWS.md",package = 'CohortOperations2'))
      ),
      shiny::tabPanel(
        title = "NEWS: CO2AnalysisModules",
        htmltools::includeMarkdown(system.file("NEWS.md",package = 'CO2AnalysisModules'))
      ),
      shiny::tabPanel(
        title = "System and Dependencies",
        shiny::h4("System version:"),
        shiny::htmlOutput(ns("systemVersion_textOutput")),
        shiny::br(),
        shiny::h4("App and AnalysisModules version:"),
        reactable::reactableOutput(ns("appVersion_reactable")),
        shiny::br(),
        shiny::h4("Dependencies version:"),
        reactable::reactableOutput(ns("dependencies_reactable"))
      ),
      shiny::tabPanel(
        title = "DatabasesConfig",
        shiny::verbatimTextOutput(ns("databasesConfig_textOutput"))
      ),
      shiny::tabPanel(
        title = "AnalysisModulesConfig",
        shiny::verbatimTextOutput(ns("analysisModulesConfig_textOutput"))
      )
    )
  )
}

#' @title mod_appVersion_server
#'
#' @description Server module for handling database selection and connection.
#'
#' @param id Shiny module ID.
#' @param databasesConfig Configuration list for databases.
#' @param r_databaseConnection Reactive values for database connection.
#'
#' @return Shiny server logic for the database selection module.
mod_appVersion_server <- function(id, databasesConfig, r_databaseConnection) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    databasesConfig <- shiny::getShinyOption("databasesConfig")
    analysisModulesConfig <- shiny::getShinyOption("analysisModulesConfig")

    rVersion  <- R.Version()
    dependenciesList <- renv::lockfile_read()$Packages
    # convert to a tibble
    dependenciesTibble <- tibble::tibble()
    for (package in names(dependenciesList)){
      dependenciesTibble <- dplyr::bind_rows(
        dependenciesTibble,
        tibble::tibble(
          Package = dependenciesList[[package]]$Package,
          Version = dependenciesList[[package]]$Version,
          Source = dependenciesList[[package]]$Source,
          RemoteRef = dependenciesList[[package]]$RemoteRef,
          Hash = dependenciesList[[package]]$Hash
        )
      )
    }

    output$systemVersion_textOutput <- shiny::renderUI({
      paste0("System version: ", rVersion$platform, "<br>",
             "R version: ", rVersion$version.string)  |>
        htmltools::HTML()
    })

    output$appVersion_reactable <- reactable::renderReactable({
      appVersion <- dependenciesTibble  |>
        dplyr::select(Package, Version, Source, RemoteRef, Hash) |>
        # TEMP hard code CO2AnalysisModules
        dplyr::filter(Package %in% c("CohortOperations2", "CO2AnalysisModules"))

      appVersion |> reactable::reactable(
        filterable = FALSE,
        pagination = FALSE
      )
    })

    output$dependencies_reactable <- reactable::renderReactable({
      dependenciesTibble |>
        dplyr::select(Package, Version, Source, RemoteRef, Hash) |>
        dplyr::arrange(Package) |>
        reactable::reactable(
          filterable = TRUE,
          pagination = TRUE
        )
    })

    output$databasesConfig_textOutput <- shiny::renderText({
      yaml::as.yaml(databasesConfig)
    })

    output$analysisModulesConfig_textOutput <- shiny::renderText({
      yaml::as.yaml(analysisModulesConfig)
    })

  })
}

