#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
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
          shinydashboard::menuItem("Import Cohorts", tabName = "importCohorts", icon = shiny::icon("address-card")),
          shinydashboard::menuItem("Match Cohorts", tabName = "matchCohorts", icon = shiny::icon("connectdevelop")),
          shinydashboard::menuItem("Subset Cohorts", tabName = "subsetCohorts", icon = shiny::icon("")),
          shinydashboard::menuItem("Export Cohorts", tabName = "exportsCohorts", icon = shiny::icon("")),
          shiny::h5(" Analyses"),
          shinydashboard::menuItem("CohortsDiagnostics", tabName = "cohortDiagnostics", icon = shiny::icon("stethoscope")),
          shinydashboard::menuItem("Cohorts Overlap", tabName = "cohortsOverlap", icon = shiny::icon("")),
          shinydashboard::menuItem("Cohorts Incidence", tabName = "cohortsIncidence", icon = shiny::icon("")),
          shinydashboard::menuItem("CodeWAS", tabName = "CodeWAS", icon = shiny::icon("")),
          shinydashboard::menuItem("Time CodeWAS", tabName = "timeCodeWAS", icon = shiny::icon("road")),
          shiny::h5(" About"),
          shinydashboard::menuItem("About", tabName = "about", icon = shiny::icon("info"))
        )
      ),

      ## BODY
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
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
              selected = "from File",
              #### panel FILE
              shiny::tabPanel(
                "from File",
                mod_importCohortsFromFile_ui("importCohortsFromFile")
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
          ## TAB Subset Cohorts
          shinydashboard::tabItem(
            tabName = "subsetCohorts",
            ### Cohorts workbench
            shinydashboard::box(
              title = "Cohorts workbench ",
              status = "primary", solidHeader = TRUE, width = 12,
              mod_cohortWorkbench_ui("cohortWorkbench_subsetCohorts")
            ),
            ### Import Cohorts
            shinydashboard::box(
              title = shiny::tagList(shiny::icon("connectdevelop"), "Subset Cohorts:"),
              solidHeader = TRUE, width = 12,
              shiny::tags$h3("Not implemented yet"),
              shiny::tags$h3("will allow to subset cohorts, based on sex, age, or other cohort patients ")
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
            ### Import Cohorts
            shinydashboard::box(
              title = shiny::tagList(shiny::icon("connectdevelop"), "Exports Cohorts:"),
              solidHeader = TRUE, width = 12,
              shiny::tags$h3("Not implemented yet"),
              shiny::tags$h3("will allow to export cohort definitions or cohorts as a cohortData file")
            )
          ),
          ##
          ## ANALYSIS
          ##

          ## TAB cohortDiagnostics
          shinydashboard::tabItem(
            tabName = "cohortDiagnostics",
            ### Cohorts workbench
            shinydashboard::box(
              title = "Cohorts workbench ",
              status = "primary", solidHeader = TRUE, width = 12,
              mod_cohortWorkbench_ui("cohortWorkbench_cohortDiagnostics")
            ),
            ### Import Cohorts
            shinydashboard::box(
              title = shiny::tagList(shiny::icon("stethoscope"), "Cohorts Diagnostics:"),
              solidHeader = TRUE, width = 12,
              mod_cohortDiagnostics_ui("cohortDiagnostics")
            )
          ),
          ## TAB cohortsOverlap
          shinydashboard::tabItem(
            tabName = "cohortsOverlap",
            ### Cohorts workbench
            shinydashboard::box(
              title = "Cohorts workbench ",
              status = "primary", solidHeader = TRUE, width = 12,
              mod_cohortWorkbench_ui("cohortWorkbench_cohortsOverlap")
            ),
            ### Import Cohorts
            shinydashboard::box(
              title = shiny::tagList(shiny::icon("connectdevelop"), "Cohorts Overlap:"),
              solidHeader = TRUE, width = 12,
              shiny::tags$h3("Not implemented yet"),
              shiny::tags$h3("will allow to compare cohorts for overlap")
            )
          ),
          ## TAB cohortsIncidence
          shinydashboard::tabItem(
            tabName = "cohortsIncidence",
            ### Cohorts workbench
            shinydashboard::box(
              title = "Cohorts workbench ",
              status = "primary", solidHeader = TRUE, width = 12,
              mod_cohortWorkbench_ui("cohortWorkbench_cohortsIncidence")
            ),
            ### Import Cohorts
            shinydashboard::box(
              title = shiny::tagList(shiny::icon("connectdevelop"), "Cohorts Incidence:"),
              solidHeader = TRUE, width = 12,
              mod_cohortsIncidence_ui("cohortsIncidence")
            )
          ),
          ## TAB CodeWAS
          shinydashboard::tabItem(
            tabName = "CodeWAS",
            ### Cohorts workbench
            shinydashboard::box(
              title = "Cohorts workbench ",
              status = "primary", solidHeader = TRUE, width = 12,
              mod_cohortWorkbench_ui("cohortWorkbench_CodeWAS")
            ),
            ### Import Cohorts
            shinydashboard::box(
              title = shiny::tagList(shiny::icon("connectdevelop"), "CodeWAS:"),
              solidHeader = TRUE, width = 12,
              shiny::tags$h3("Not implemented yet"),
              shiny::tags$h3("will allow to copute codewas")
            )
          ),
          ## TAB timeCodeWAS
          shinydashboard::tabItem(
            tabName = "timeCodeWAS",
            ### Cohorts workbench
            shinydashboard::box(
              title = "Cohorts workbench ",
              status = "primary", solidHeader = TRUE, width = 12,
              mod_cohortWorkbench_ui("cohortWorkbench_timeCodeWAS")
            ),
            ### Import Cohorts
            shinydashboard::box(
              title = shiny::tagList(shiny::icon("connectdevelop"), "TimeCodeWAS:"),
              solidHeader = TRUE, width = 12,
              mod_timeCodeWAS_ui("timeCodeWAS")
            )
          ),
           shinydashboard::tabItem(
              tabName = "about",
              shinydashboard::box(
                  title = "About",
                  shiny::uiOutput("about"),
                  width = 12
              )
            )
        )
      )
    )
  )
}
