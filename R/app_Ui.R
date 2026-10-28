#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar sidebarMenu menuItem tabItems tabItem box tabBox
#' @importFrom stringr str_to_title
#' @importFrom htmltools tagAppendChild
#' @noRd
app_ui <- function(request) {

  # get settings loaded from file
  analysisModulesConfig <- shiny::getShinyOption("analysisModulesConfig")

  #
  # Build tabBox for import cohorts
  #
  # Build list of all tab panels
  allTabPanels <- list(
    #### panel FILE
    shiny::tabPanel(
      "from File",
      mod_importCohortsFromFile_ui("importCohortsFromFile")
    ),
    #### panel ATLAS
    shiny::tabPanel(
      "from Atlas",
      mod_importCohortsFromAtlas_ui("importCohortsFromAtlas")
    )
  )
  
  # Add dynamic panels for import from libraries
  importFromLibrariesConfig <- analysisModulesConfig$importFromLibraries
  if (!is.null(importFromLibrariesConfig)) {
    for (libraryKey in names(importFromLibrariesConfig)) {
      libraryConfig <- importFromLibrariesConfig[[libraryKey]]
      moduleId <- paste0("importCohortsFrom", stringr::str_to_title(libraryKey))
      allTabPanels[[length(allTabPanels) + 1]] <- shiny::tabPanel(
        paste0("from ", libraryConfig$name),
        mod_importCohortsFromCohortsTable_ui(moduleId)
      )
    }
  }
  
  # Build tabBox with all panels using eval/call to avoid do.call
  # Construct the function call with all panels as arguments
  tabBoxCall <- call(
    "tabBox",
    title = shiny::tagList(shiny::icon("upload"), "Import Cohorts:"),
    id = "import_files",
    width = 12L,
    side = "right",
    selected = "from Atlas"
  )
  
  # Add all panels to the call
  for (panel in allTabPanels) {
    tabBoxCall[[length(tabBoxCall) + 1]] <- panel
  }
  
  # Evaluate the call with tabBox function in environment
  tabBoxResult <- eval(
    tabBoxCall,
    envir = list(tabBox = shinydashboard::tabBox),
    enclos = environment()
  )

  #
  # Build list of tabs
  # 

  listOfStaticTabItems  <- list(
    ## Connect to databases
    shinydashboard::tabItem(
      tabName = "selectDatabases",
      mod_selectDatabases_ui("selectDatabases")
    ),
    ## TAB Import Cohorts
    shinydashboard::tabItem(
      tabName = "importCohorts",
      ### Cohorts workbench
      shinydashboard::box(
        title = "Cohorts workbench ",
        status = "primary", solidHeader = TRUE, width = 12,
        mod_cohortWorkbench_ui("cohortWorkbench_importCohorts")
      ),
      ### Import Cohorts
      tabBoxResult
    ),
    ## TAB Matching Cohorts
    shinydashboard::tabItem(
      tabName = "matchCohorts",
      ### Cohorts workbench
      shinydashboard::box(
        title = "Cohorts workbench ",
        status = "primary", solidHeader = TRUE, width = 12,
        mod_cohortWorkbench_ui("cohortWorkbench_matchCohorts")
      ),
      ### Import Cohorts
      shinydashboard::box(
        title = shiny::tagList(shiny::icon("connectdevelop"), "Match Cohorts:"),
        solidHeader = TRUE, width = 12,
        mod_matchCohorts_ui("matchCohorts")
      )
    ),
    ## TAB Operate Cohorts
    shinydashboard::tabItem(
      tabName = "OperateCohorts",
      ### Cohorts workbench
      shinydashboard::box(
        title = "Cohorts workbench ",
        status = "primary", solidHeader = TRUE, width = 12,
        mod_cohortWorkbench_ui("cohortWorkbench_operateCohorts")
      ),
      ### Import Cohorts
      shinydashboard::box(
        title = shiny::tagList(shiny::icon("calculator"), "Operate Cohorts:"),
        solidHeader = TRUE, width = 12,
        mod_operateCohorts_ui("operateCohorts")
      )
    ),
    ## TAB Exports Cohorts
    shinydashboard::tabItem(
      tabName = "exportsCohorts",
      ### Cohorts workbench
      shinydashboard::box(
        title = "Cohorts workbench ",
        status = "primary", solidHeader = TRUE, width = 12,
        mod_cohortWorkbench_ui("cohortWorkbench_exportsCohorts")
      ),
      ### Export Cohorts
      shinydashboard::box(
        title = shiny::tagList(shiny::icon("file-export"), "Exports Cohorts:"),
        solidHeader = TRUE, width = 12,
        mod_exportsCohorts_ui("exportsCohorts")
      )
    ),
    ## TAB App Version
    shinydashboard::tabItem(
      tabName = "appVersion",
      shinydashboard::box(
        title = "App News",
        status = "primary", solidHeader = TRUE, width = 12,
        mod_appVersion_ui("appVersion")
      )
    ),
    ## TAB View Results
    shinydashboard::tabItem(
      tabName = "viewResults",
      shinydashboard::box(
        title = "View Results",
        status = "primary", solidHeader = TRUE, width = 12,
        mod_viewResults_ui("viewResults")
      )
    ),
    ## TAB Help
    shinydashboard::tabItem(
      tabName = "help",
      shinydashboard::box(
        title = shiny::tagList(shiny::icon("question-circle"), "Using Cohort Operations"),
        status = "primary", solidHeader = TRUE, width = 12,
        htmltools::div(
          class = "co2-help-page",
          htmltools::includeMarkdown(
            system.file("appDocumentation/using_co2.md", package = "CohortOperations2")
          )
        )
      )
    )
  )


  # Dynamically Generated Tabs for ANALYSIS
  analysisKeys <- setdiff(names(analysisModulesConfig), "importFromLibraries")
  listOfDynanicTabs <- lapply(analysisKeys, function(analysisKey) {
    analysis <- analysisModulesConfig[[analysisKey]]
    shinydashboard::tabItem(
      tabName = analysisKey,
      ### Cohorts workbench
      shinydashboard::box(
        title = "Cohorts workbench ",
        status = "primary", solidHeader = TRUE, width = 12,
        mod_cohortWorkbench_ui(paste0("cohortWorkbench_", analysisKey))
      ),
      ### Analysis specific box
      shinydashboard::box(
        title = shiny::tagList(shiny::icon(analysis$icon), analysis$analysisName),
        solidHeader = TRUE, width = 12,
        mod_analysisWrap_ui(paste0(analysisKey, "_analysis"), analysis$mod_analysisSettings_ui |> fct_stringToFuction())
      )
    )
  })


  listOfTabs <- c(listOfStaticTabItems, listOfDynanicTabs)

  #
  # Build dashboard
  #

  shiny::tagList(
    shiny::tags$style(htmltools::HTML("
      .co2-help-page {
        padding: 0 12px 12px;
      }
      .co2-help-page ul,
      .co2-help-page ol {
        margin-left: 0;
        padding-left: 1.5em;
      }
      .co2-help-page li {
        padding-left: 0.15em;
      }
      .co2-inline-help {
        float: right;
        position: relative;
        z-index: 20;
        margin: 0 0 0.5rem 0.75rem;
      }
      .co2-inline-help-trigger {
        cursor: pointer;
        color: #337ab7;
        font-size: 1.35em;
        line-height: 1;
        list-style: none;
      }
      .co2-inline-help-trigger::-webkit-details-marker {
        display: none;
      }
      .co2-inline-help-trigger:hover,
      .co2-inline-help-trigger:focus {
        color: #23527c;
      }
      .co2-inline-help-content {
        position: absolute;
        top: 1.8rem;
        right: 0;
        width: min(460px, 80vw);
        padding: 12px 14px;
        background: #fff;
        border: 1px solid #b8c7ce;
        border-radius: 4px;
        box-shadow: 0 3px 10px rgba(0, 0, 0, 0.18);
      }
      .co2-inline-help-content p:last-child {
        margin-bottom: 0;
      }
      .co2-field-label {
        display: flex;
        align-items: center;
        gap: 0.35rem;
        font-weight: 600;
        margin-bottom: 0.25rem;
      }
      .co2-section-heading {
        display: flex;
        align-items: center;
        gap: 0.4rem;
      }
      .co2-section-heading h4 {
        margin-top: 10px;
        margin-bottom: 10px;
      }
      .co2-field-help {
        position: relative;
        display: inline-block;
        z-index: 15;
      }
      .co2-field-help-trigger {
        cursor: pointer;
        color: #337ab7;
        list-style: none;
        line-height: 1;
      }
      .co2-field-help-trigger::-webkit-details-marker {
        display: none;
      }
      .co2-field-help-trigger:hover,
      .co2-field-help-trigger:focus {
        color: #23527c;
      }
      .co2-field-help-content {
        position: absolute;
        top: 1.5rem;
        left: 0;
        width: min(360px, 70vw);
        padding: 0.7rem 0.85rem;
        background: #fff;
        border: 1px solid #b8c7ce;
        border-radius: 4px;
        box-shadow: 0 3px 10px rgba(0, 0, 0, 0.18);
        font-weight: normal;
      }
      .co2-field-help-content p:last-child {
        margin-bottom: 0;
      }
    ")),
    # Your application UI logic
    shinydashboard::dashboardPage(

      # TITLE
      shinydashboard::dashboardHeader(title = "Cohort Operations"),

      ## SIDEBAR
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shiny::h5(" Databases"),
          shinydashboard::menuItem("Databases connection", tabName = "selectDatabases", icon = shiny::icon("database")),
          shiny::h5(" Cohorts"),
          shinydashboard::menuItem("Import Cohorts", tabName = "importCohorts", icon = shiny::icon("upload")),
          shinydashboard::menuItem("Operate Cohorts", tabName = "OperateCohorts", icon = shiny::icon("calculator")),
          shinydashboard::menuItem("Match Cohorts", tabName = "matchCohorts", icon = shiny::icon("connectdevelop")),
          shinydashboard::menuItem("Export Cohorts", tabName = "exportsCohorts", icon = shiny::icon("file-export")),
          shiny::h5(" Analyses"),
          # Dynamically generated menu items
          {
            analysisKeys <- setdiff(names(analysisModulesConfig), "importFromLibraries")
            lapply(analysisKeys, function(analysisKey) {
              analysis <- analysisModulesConfig[[analysisKey]]
              shinydashboard::menuItem(
                text = analysis$analysisName,
                tabName = analysisKey,
                icon = shiny::icon(analysis$icon)
              )
            })
          },
          shiny::h5(" Results"),
          shinydashboard::menuItem("View Results", tabName = "viewResults", icon = shiny::icon("eye")),
          shiny::h5(" About"),
          shinydashboard::menuItem("App News", tabName = "appVersion", icon = shiny::icon("info-circle")),
          shinydashboard::menuItem("Help", tabName = "help", icon = shiny::icon("question-circle")),
          shinydashboard::menuItem("App Logs", icon = shiny::icon("info-circle"), href = "/logs/log.txt")
        )
      ),

      ## BODY
      shinydashboard::dashboardBody(
        do.call(shinydashboard::tabItems, listOfTabs)
      )
    ),
      # Open logfile if app crashes, to avoid shiny grey screen
    .showLogWhenDisconnected()

  )
}
