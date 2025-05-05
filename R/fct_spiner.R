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

  showModal(modalDialog(
    shiny::tags$div(
      message,
      ui_load_spinner(shiny::plotOutput(outputId = "plot", width = "100px", height = "100px"), proxy.height = "90px"),
      shiny::HTML(paste0(
        "<script>
        (function() {
          let latestTimestamp = null;
          window.lastShownLine = window.lastShownLine || null;
          let isFetching = false;

          function updateText(isInitial) {
            if (isFetching) return;
            isFetching = true;

            if (isInitial) {
                window.lastShownLine = null;
              }

            fetch('", logUrl, "').then(response => {
                if (!response.ok) throw new Error('Network response was not OK');
                return response.text();
              }).then(text => {
                const textLines = String(text).split('\\n').filter(line => line.trim() !== '');

                // Find the most recent '[Task Complete]' line
                const lastTaskCompleteIndex = [...textLines].reverse().findIndex(line => line.includes('[Task Complete]'));
                const cutoffIndex = lastTaskCompleteIndex >= 0 ? textLines.length - lastTaskCompleteIndex : 0;

                const filteredLines = textLines.slice(cutoffIndex);

                // Find new lines after lastShownLine
                let newEntries = [];
                if (window.lastShownLine !== null) {
                  const lastIndex = filteredLines.findIndex(line => line === window.lastShownLine);
                  if (lastIndex >= 0 && lastIndex < filteredLines.length - 1) {
                    newEntries = filteredLines.slice(lastIndex + 1);
                  } else if (lastIndex === -1) {
                    // lastShownLine not found, maybe file reset
                    newEntries = filteredLines;
                  }
                } else {
                  newEntries = filteredLines;
                }

                // Update latestTimestamp from new entries
                const timestamps = newEntries.map(line => {
                    const match = line.match(/^(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})/);
                    return match ? new Date(match[1]) : null;
                  }).filter(ts => ts !== null);

                if (timestamps.length > 0) {
                  latestTimestamp = new Date(Math.max(...timestamps.map(ts => ts.getTime())));
                }

                const logContainer = document.getElementById('updatedText');
                if (logContainer && newEntries.length > 0) {
                  const strippedEntries = newEntries.map(line => {
                    const trimmedLine = line.trim();
                    const match = trimmedLine.match(/^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\t(.+)$/);
                    return match ? match[1] : trimmedLine;  // fallback to full line if no match. To show log entries with timestamp, match[0]

                   });
                  const prefix = logContainer.textContent.trim().length > 0 ? '\\n' : '';
                  logContainer.textContent += prefix + strippedEntries.join('\\n');
                  window.lastShownLine = newEntries[newEntries.length - 1];
                }

                isFetching = false;
              })
              .catch(error => {
                console.error('Fetch error:', error);
                isFetching = false;
              });
          }

          updateText(true);
          setInterval(() => updateText(false), ", as.integer(updateMiliseconds), ");
        })();
      </script>
      <pre id='updatedText' style='border: 1px solid #ccc; padding: 10px; text-align: left; min-height: 300px; max-height: 300px; overflow-y: auto; display: block; white-space: pre-wrap;'></pre>"
      ))
      # attendantBar("progress-bar", hidden = TRUE, max=1000)
    ),
    title = NULL,
    footer = NULL,
    easyClose = F,
    fade = TRUE,
    width = "550px",
    ...
  ))

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
  ParallelLogger::logInfo("[Task Complete]")
  removeModal()
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
