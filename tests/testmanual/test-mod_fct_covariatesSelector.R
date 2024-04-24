
# run module --------------------------------------------------------------
devtools::load_all(".")


analysisIdsToShow <- HadesExtras::getListOfAnalysis() |> dplyr::pull(analysisId)
analysisIdsToShow <- analysisIdsToShow[1:30]
analysisIdsSelected <- analysisIdsToShow[1:3]


shiny::shinyApp(
  shiny::fluidPage(
    mod_fct_covariateSelector_ui(
      inputId = "test",
      label = "This is a test lable",
      analysisIdsToShow = analysisIdsToShow,
      analysisIdsSelected = analysisIdsSelected
      )
  ),
  function(input,output,session){


    observe({
      print(input$test)
    })

  },
  options = list(launch.browser=TRUE)
)
