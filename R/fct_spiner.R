#' ui_load_spinner
#'
#' Adds dna spinner to a ui_element
#'
#' @param ui_element ui_element to add spinner to
#' @param ... other parameters to shinycustomloader::withLoader
#'
#' @importFrom shinycustomloader withLoader
#'
#' @export
ui_load_spinner <- function(ui_element, ...) {
  shinycustomloader::withLoader(
    ui_element,
    type = "html",
    loader = "dnaspin",
    ...
  )
}

#' fct_sweetAlertSpinner
#'
#' A sweet alert with a dna spinner
#'
#' @param message  message to show in the alert
#' @param logUrl URL to fetch text from
#' @param updateMiliseconds time to update the text
#' @param ... additional options for shinyWidgets::show_alert
#'
#' @export
#'
#'
#' @importFrom shinyWidgets show_alert
fct_sweetAlertSpinner <- function(message, logUrl = "/logs/log.txt", updateMiliseconds = 500, ...) {

  shinyWidgets::show_alert(
    title = NULL,
    text = shiny::tags$div(
      message,
      ui_load_spinner(shiny::plotOutput(outputId = "plot", width = "100px", height = "100px"), proxy.height = "90px"),
      shiny::HTML(paste0(
        "<script>
          function updateText() {
              fetch('", logUrl, "')
                .then(response => {
                  if (!response.ok) {
                    throw new Error('Network response was not OK', response);
                  }
                  return response.text();
                })
                .then(text => {
                  // console.log('Text from URL:', text);
                  // Do something with the text here
                  text = String(text);
                  const lastlines = text.split('\\n').slice(-10).join('\\n');

                  if (document.getElementById('updatedText')){
                    document.getElementById('updatedText').innerHTML = lastlines;
                  }

                })
                .catch(error => {
                  console.error('There was a problem fetching the text:', error);
                });
          }

          // Update text every 5 seconds
          setInterval(updateText, ", as.integer(updateMiliseconds),");

          // Call updateText initially to update the text when the page loads
          updateText();
        </script>
        <pre id='updatedText' style='border: 1px solid #ccc; padding: 10px; text-align: left;'></pre>"
      ))
      # attendantBar("progress-bar", hidden = TRUE, max=1000)
    ),
    html = TRUE,
    type = NULL,
    btn_labels = NA,
    closeOnClickOutside = FALSE,
    showCloseButton = FALSE,
    width = "550px",
    ...
  )

  # if(!is.null(wait_time_sec)){
  #   att <- Attendant$new("progress-bar")
  #   att$auto(ms= wait_time_sec)
  #   att$set(1)
  # }
}

#' fct_removeSweetAlertSpinner
#'
#' Closes a fct_sweetAlertSpinner
#'
#' @export
#'
#' @importFrom shinyWidgets closeSweetAlert
fct_removeSweetAlertSpinner <- function() {
  shinyWidgets::closeSweetAlert()
}




.listToString <- function(list) {
  # string with list of names = values separated by :
  paste0(
    names(list),
    " = ",
    unlist(list),
    collapse = " ; "
  )
}
