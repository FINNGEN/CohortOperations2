


DatabaseConnector::connect(
  dbms = "bigquery",
  user = "",
  password = "",
  connectionString = "jdbc:bigquery://https://www.googleapis.com/bigquery/v2:443;ProjectId=finngen-logs;OAuthType=0;OAuthServiceAcctEmail=146473670970-compute@developer.gserviceaccount.com;OAuthPvtKeyPath=/Users/javier/keys/atlas-development-270609-410deaacc58b.json;Timeout=100000;",
  pathToDriver = "/Users/javier/.config/hades/bigquery"
)


"
SELECT
  resource.labels.project_id AS sandbox,
  resource.labels.instance_id,
  timestamp,
  SUBSTR(jsonPayload.message, INSTR(jsonPayload.message, '[sandbox-co2-log]')) AS message
FROM `finngen-logs.sandbox_logs.syslog` WHERE
 resource.labels.project_id = 'fg-production-sandbox-6' AND
 STRPOS(jsonPayload.message, '[sandbox-co2-log]')>0
 ORDER BY  timestamp DESC LIMIT 1000
"



logs <- readr::read_csv('~/Downloads/bquxjob_7d7d3f8d_18f9fe3db6e.csv')

logs |>
  dplyr::mutate(
    message = stringr::str_replace_all(message, "#011", '\t'),
    start = stringr::str_detect(message, 'Start logging on Branch')
  )  |>
  # create a new column with a number that increases when the start of a new log is detected
  dplyr::mutate(
    log = cumsum(start)
  )   |>
  View()
