#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar sidebarMenu menuItem tabItems tabItem box tabBox
#' @noRd
app_ui <- function(request) {

  # get settings loaded from file
  analysisModulesConfig <- shiny::getShinyOption("analysisModulesConfig")


  #
  # Build list of tabs
  # (small hack to allow dynamic generation of tabs)

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
      shinydashboard::tabBox(
        title = shiny::tagList(shiny::icon("upload"), "Import Cohorts:"),
        id = "import_files", width = 12, side="right",
        selected = "from Atlas",
        #### panel FILE
        shiny::tabPanel(
          "from File",
          mod_importCohortsFromFile_ui("importCohortsFromFile")
        ),
        #### panel ATLAS
        shiny::tabPanel(
          "from Atlas",
          mod_importCohortsFromAtlas_ui("importCohortsFromAtlas")
        ),
        #### panel ENDPOINTs
        shiny::tabPanel(
          "from Endpoints",
          mod_importCohortsFromCohortsTable_ui("importCohortsFromEndpoints")
        ),
        #### panel Library
        shiny::tabPanel(
          "from Library",
          mod_importCohortsFromCohortsTable_ui("importCohortsFromLibrary")
        )
      )
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
    )
  )


  # Dynamically Generated Tabs for ANALYSIS
  listOfDynanicTabs <- lapply(names(analysisModulesConfig), function(analysisKey) {
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
          lapply(names(analysisModulesConfig), function(analysisKey) {
            analysis <- analysisModulesConfig[[analysisKey]]
            shinydashboard::menuItem(
              text = analysis$analysisName,
              tabName = analysisKey,
              icon = shiny::icon(analysis$icon)
            )
          }),
          shiny::h5(" Results"),
          shinydashboard::menuItem("View Results", tabName = "viewResults", icon = shiny::icon("eye")),
          shiny::h5(" About"),
          shinydashboard::menuItem("App News", tabName = "appVersion", icon = shiny::icon("info-circle")),
          shinydashboard::menuItem("App Logs", icon = shiny::icon("info-circle"), href = "/logs/log.txt")
        )
      ),

      ## BODY
      shinydashboard::dashboardBody(
        do.call(shinydashboard::tabItems, listOfTabs)
      )
    )
  )
}
