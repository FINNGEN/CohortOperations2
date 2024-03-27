#!/bin/bash
source /etc/sandbox/env
export DOCKER_HOST=unix://$XDG_RUNTIME_DIR/docker.sock
notify-send 'Cohort Operations 2' 'We are starting your application. This could take up to 1min. Browser will be started automatically when ready.'

# update images
docker pull eu.gcr.io/finngen-sandbox-v3-containers/co2:latest
docker pull eu.gcr.io/finngen-sandbox-v3-containers/cow:latest

# get values from enviromental variables
# if envar SANDBOX_PROJECT is not set, trown an error
if [ -z "$SANDBOX_PROJECT" ]; then
    echo "Error: environmental variable SANDBOX_PROJECT is not set"
    exit 1
fi

# if envar SESSION_MANAGER is not set, trown an error, if it is set extract the numreic id between ivm and :
if [ -z "$SESSION_MANAGER" ]; then
    echo "Error: environmental variable SESSION_MANAGER is not set"
    exit 1
else
    SESSION_MANAGER_ID=$(echo $SESSION_MANAGER | grep -oP 'ivm-\K[0-9]+' | head -1)
fi

# create config files
echo "
urlCohortOperationsViewer: http://127.0.0.1:8560/?pathToResultsZip=
" > /tmp/co2_config.yml

echo "
DF12:
  cohortTableHandler:
    database:
      databaseId: DF12
      databaseName: FinngGen-DF12
      databaseDescription: FinnGen database 12
    connection:
      connectionDetailsSettings:
        dbms: bigquery
        user: ''
        password: ''
        connectionString: jdbc:bigquery://https://www.googleapis.com/auth/bigquery:433;ProjectId="$SANDBOX_PROJECT";OAuthType=3;Timeout=10000;
        pathToDriver: /root/jdbc_drivers/bigquery
      tempEmulationSchema: "$SANDBOX_PROJECT".sandbox #needed for creating tmp table in BigQuery
      useBigrqueryUpload: true # option for HadesExtras
    cdm:
      cdmDatabaseSchema: finngen-production-library.finngen_omop_r12
      vocabularyDatabaseSchema: finngen-production-library.finngen_omop_r12
    cohortTable:
      cohortDatabaseSchema: "$SANDBOX_PROJECT".sandbox
      cohortTableName: co2_d12_"$SESSION_MANAGER_ID"

DF11:
  cohortTableHandler:
    database:
      databaseId: DF11
      databaseName: FinngGen-DF11
      databaseDescription: FinnGen database 11
    connection:
      connectionDetailsSettings:
        dbms: bigquery
        user: ''
        password: ''
        connectionString: jdbc:bigquery://https://www.googleapis.com/auth/bigquery:433;ProjectId="$SANDBOX_PROJECT";OAuthType=3;Timeout=10000;
        pathToDriver: /root/jdbc_drivers/bigquery
      tempEmulationSchema: "$SANDBOX_PROJECT".sandbox #needed for creating tmp table in BigQuery
      useBigrqueryUpload: true # option for HadesExtras
    cdm:
      cdmDatabaseSchema: finngen-production-library.finngen_omop_r11
      vocabularyDatabaseSchema: finngen-production-library.finngen_omop_r11
    cohortTable:
      cohortDatabaseSchema: "$SANDBOX_PROJECT".sandbox
      cohortTableName: co2_d11_"$SESSION_MANAGER_ID"

" > /tmp/co2_databases_config.yml


docker run --log-driver syslog -d -p 8559:8888 -v /tmp:/tmp  \
    -e CO2_CONFIG_FILE="/tmp/co2_config.yml"  -e CO2_DATABASES_CONFIG_FILE="/tmp/co2_databases_config.yml" \
    eu.gcr.io/finngen-sandbox-v3-containers/co2:latest

docker run --log-driver syslog -d -p 8560:8888 -v /tmp:/tmp  \
    eu.gcr.io/finngen-sandbox-v3-containers/cow:latest

echo "if the cohort operations does not launch automatically, visit: http://localhost:8556"
#sleep 10
until [ "$(curl -s -o /dev/null -I -w '%{http_code}' "http://localhost:8559")" -eq 200 ]
do
  sleep 5
done
firefox --new-tab 'http://localhost:8559'
