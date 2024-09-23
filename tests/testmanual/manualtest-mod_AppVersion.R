# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))


# run module --------------------------------------------------------------
devtools::load_all(".")

app <- shiny::shinyApp(
  shiny::fluidPage(
    mod_appVersion_ui("select_configuration")
  ),
  function(input,output,session){
    mod_appVersion_server("select_configuration")
  },
  options = list(launch.browser=TRUE)
)


app

