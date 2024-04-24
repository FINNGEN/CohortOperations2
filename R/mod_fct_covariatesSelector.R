

mod_fct_covariateSelector_ui <- function(inputId, label = NULL, analysisIdsToShow = NULL, analysisIdsSelected = NULL) {


  if(is.null(analysisIdsToShow)) {
    analysisIdsToShow  <- HadesExtras::getListOfAnalysis()$analysisId
  }
  if(is.null(analysisIdsSelected)) {
    analysisIdsSelected <- analysisIdsToShow
  }

  checkmate::assertNumeric(analysisIdsToShow)
  checkmate::assertNumeric(analysisIdsSelected)
  checkmate::assertSubset(analysisIdsToShow, HadesExtras::getListOfAnalysis()$analysisId)
  checkmate::assertSubset(analysisIdsSelected, analysisIdsToShow)

  analysisNamePretty  <- tibble::tribble(
    ~analysisName, ~analysisNamePretty,
    "ConditionOccurrence", "Conditions",
    "ConditionOccurrencePrimaryInpatient", "Conditions in Primary Inpatient",
    "ConditionEraGroupOverlap", "Conditions SNOMED Group",
    "DrugExposure", "Drugs",
    "DrugEraGroupOverlap", "Drugs ATC Group",
    "ProcedureOccurrence", "Procedures",
    "DeviceExposure", "Device Exposure",
    "Measurement", "Measurement",
    "Observation", "Observation"
  )

  analysisToShow <- HadesExtras::getListOfAnalysis()  |>
    dplyr::left_join(analysisNamePretty, by = c("analysisName" = "analysisName")) |>
    dplyr::mutate(analysisNamePretty = dplyr::if_else(is.na(analysisNamePretty), analysisName, analysisNamePretty)) |>
    dplyr::mutate(analysisNamePretty = stringr::str_replace_all(analysisNamePretty, "([a-z])([A-Z])", "\\1 \\2")) |>
    dplyr::filter(analysisId %in% analysisIdsToShow) |>
    dplyr::select(group = domainId, analysisId, analysisNamePretty)

  # named list with group names and sublist of analysisNamePretty and analysisId
  choices <- analysisToShow |>
   tidyr::nest(data = -group) |>
    dplyr::mutate(
      data = purrr::map(data, ~{
        list   <- as.list(.x$analysisId)
        names(list) <- .x$analysisNamePretty
        return(list)
      })
    )

  choicesList  <- as.list(choices$data)
  names(choicesList) <- choices$group

  picker <-  shinyWidgets::pickerInput(
      inputId = inputId,
      label = label,
      choices = choicesList,
      selected = analysisIdsSelected,
      options = list(`actions-box` = TRUE),
      multiple = TRUE)

  return(picker)

}




