

# run module --------------------------------------------------------------
devtools::load_all(".")

shiny::shinyApp(
  shiny::fluidPage(
    mod_formTimeWindows_ui("test")
  ),
  function(input,output,session){
    output <- mod_formTimeWindows_server("test", session)

    shiny::observe({
        print(output())
    })

  },
  options = list(launch.browser=TRUE)
)
