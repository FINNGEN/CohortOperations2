# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))

logger <- fcr_setUpLogger()

r_connectionHandlers <- shiny::reactiveValues(
  databasesHandlers = NULL
)


# run module --------------------------------------------------------------
devtools::load_all(".")

app <- shiny::shinyApp(
  shiny::fluidPage(
    mod_selectDatabases_ui("select_configuration")
  ),
  function(input,output,session){
    mod_selectDatabases_server("select_configuration", databasesConfig, r_connectionHandlers)
  },
  options = list(launch.browser=TRUE)
)

app$appOptions$logger  <- logger
app



