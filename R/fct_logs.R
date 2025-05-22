
#' Set Up Logger
#'
#' Sets up a logger with console and file appenders for logging.
#'
#' @return A logger object.
#'
#' @export
fcr_setUpLogger  <- function(){

  timestamp  <- timestamp <- as.character(as.numeric(format(Sys.time(), "%d%m%Y%H%M%OS2"))*100)
  folderWithLog <- file.path(tempdir(),  paste0("logs", timestamp))
  dir.create(folderWithLog, showWarnings = FALSE)
  logger <- ParallelLogger::createLogger(
    threshold = "TRACE",
    appenders = list(
      # to console for tracking
      .createConsoleAppenderForSandboxLogging(),
      # to file for showing in app
      ParallelLogger::createFileAppender(
        fileName = file.path(folderWithLog, "log.txt"),
        layout = ParallelLogger::layoutTimestamp
      )
    )
  )
  ParallelLogger::clearLoggers()
  #addDefaultFileLogger(file.path(folderWithLog, "log2.txt"))
  ParallelLogger::registerLogger(logger)

  shiny::addResourcePath("logs", folderWithLog)

}


#' Create Console Appender for Sandbox Logging
#'
#' Creates a console appender for sandbox logging with a specified layout.
#'
#' @param layout A layout function for the logger. Defaults to ParallelLogger::layoutParallel.
#'
#' @return An appender object for logging.
#'
.createConsoleAppenderForSandboxLogging <- function(layout = ParallelLogger::layoutParallel) {
  appendFunction <- function(this, level, message, echoToConsole) {
    # Avoid note in check:
    missing(this)
    message <- paste0("[sandbox-co2-log] ", message)
    writeLines(message, con = stderr())
  }
  appender <- list(appendFunction = appendFunction, layout = layout)
  class(appender) <- "Appender"
  return(appender)
}

.showLogWhenDisconnected <- function() {
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$style(HTML("
        #custom-disconnect-overlay {
          position: fixed;
          top: 0; left: 0; right: 0; bottom: 0;
          background-color: rgba(0, 0, 0, 0.6);
          z-index: 9998;
          display: none;
        }
        #custom-disconnect-dialog {
          position: fixed;
          top: 10%;
          left: 50%;
          transform: translateX(-50%);
          background-color: white;
          padding: 20px;
          font-size: 16px;
          border-radius: 8px;
          z-index: 9999;
          width: 80%;
          max-height: 80%;
          overflow: auto;
          box-shadow: 0 0 15px rgba(0, 0, 0, 0.4);
          display: none;
        }
        #custom-log-content {
          text-align: left;
          font-family: monospace;
          white-space: pre-wrap;
          max-height: 60vh;
          overflow-y: auto;
          border: 1px solid #ccc;
          padding: 10px;
          background-color: #f7f7f7;
          margin-top: 10px;
        }
      ")),
      shiny::tags$script(HTML("
        $(document).on('shiny:disconnected', function(event) {
          console.error('Shiny disconnected. Fetching log file...');
          $('#custom-disconnect-overlay').show();
          $('#custom-disconnect-dialog').show();
          $('#custom-log-content').text('Loading log file...');

          fetch('/logs/log.txt')
            .then(response => response.text())
            .then(text => {
              const lines = text.trim().split('\\n');
              const last10 = lines.slice(-10).join('\\n');
              $('#custom-log-content').text(last10);
            })
            .catch(error => {
              $('#custom-log-content').text('Could not load log file.');
              console.error('Error loading log:', error);
            });
        });

        $(document).on('shiny:error', function(event) {
         console.error('Shiny error. Fetching log file...');
         $('#custom-disconnect-overlay').show();
         $('#custom-disconnect-dialog').show();
         $('#custom-log-content').text('Loading log file...');

         fetch('/logs/log.txt')
           .then(response => response.text())
           .then(text => {
             const lines = text.trim().split('\\n');
             const last10 = lines.slice(-10).join('\\n');
             $('#custom-log-content').text(last10);
           })
           .catch(error => {
             $('#custom-log-content').text('Could not load log file.');
             console.error('Error loading log:', error);
           });
       });

      "))
    ),

    # The overlay and dialog box HTML
    shiny::tags$div(id = "custom-disconnect-overlay"),
    shiny::tags$div(id = "custom-disconnect-dialog",
    shiny::tags$div("You've been disconnected due to inactivity or error. The log file content is shown below for troubleshooting:"),
    shiny::tags$div(id = "custom-log-content")
    )
  )
}


