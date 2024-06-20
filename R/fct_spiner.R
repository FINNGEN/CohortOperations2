#' ui_load_spinner
#'
#' Adds dna spinner to a ui_element
#'
#' @param ui_element ui_element to add spinner to
#' @param ... other parameters to shinycustomloader::withLoader
#'
#' @return
#' @export
#'
#' @importFrom shinycustomloader withLoader
ui_load_spinner <- function(ui_element, ...) {
  shinycustomloader::withLoader(
    ui_element,
    type = "html",
    loader = "dnaspin",
    ...
  )
}

#' sweetAlert_spinner
#'
#' A sweet alert with a dna spinner
#'
#' @param message  message to show in the alert
#' @param wait_time_sec if not NULL shows a progress bar that loads in the given second
#' @param ... additional options for shinyWidgets::show_alert
#'
#' @return
#' @export
#'
#'
#' @importFrom shinyWidgets show_alert
sweetAlert_spinner <- function(message, logUrl = "/logs/log.txt", updateMiliseconds = 2000, ...) {

  sessionTimestamp <- Sys.time()
  logId <- .findPrevSessionLatestLogLineId(sessionTimestamp)

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

                  // if there is somewhere in the code another created spinner that was not terminated
                  // the logs will be printed from multiple processes to the same spinner which could
                  // mess up the view since the logId could be different in these cases
                  const id = '", logId, "';

                  const regex = new RegExp('[1-9][0-9][0-9][0-9]-(0[1-9]|1[0-2])-(0[1-9]|1[0-9]|2[0-9]|3[0-1]) [0-9][0-9]:[0-9][0-9]:[0-9][0-9]\t');
                  const lines = text.split('\\n').filter((el, i) => i >= Number(id)).map(el => el.replace(regex, ''));
                  const lastlines = lines.slice(-10).join('\\n');

                  if (document.getElementById('updatedText')){
                    document.getElementById('updatedText').innerHTML = lastlines;
                  }

                })
                .catch(error => {
                  console.error('There was a problem fetching the text:', error);
                });
          }

          // Update text every 2 seconds
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

#' remove_sweetAlert_spinner
#'
#' Closes a sweetAlert_spinner
#'
#' @return
#' @export
#'
#' @importFrom shinyWidgets closeSweetAlert
remove_sweetAlert_spinner <- function() {
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


.findPrevSessionLatestLogLineId <- function(sessionTimestamp){

  loggers <- ParallelLogger::getLoggers()
  fileName <- NULL
  for (l in loggers){
    for (a in l$appenders){
      if ('fileName' %in% names(a)){
        fileName <- a$fileName
      }
    }
  }

  lines <- readLines(fileName)
  logs <- sapply(lines, strsplit, "\t")

  # calculate time difference
  td <- sapply(1:length(logs), function(k)
    as.numeric(difftime(
      sessionTimestamp,
      strptime(logs[[k]][1], format = "%Y-%m-%d %H:%M:%S"),
      units='mins'))
  )

  latestLogId <- order(td, decreasing = F)[1]

  return (latestLogId)

}

