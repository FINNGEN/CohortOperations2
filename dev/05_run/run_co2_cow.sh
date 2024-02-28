#!/bin/bash


# Function to display usage information
display_usage() {
    echo "Usage: $0 [-tag <docker_image_tag>] "
    echo "  -co2_tag <docker_image_tag> : The tag of the co2 docker image to use. Default is 'latest' "
    echo "  -co2_port <port> : The port to use for the co2 container. Default is 9999 "
    echo "  -cow_tag <docker_image_tag> : The tag of the cow docker image to use. Default is 'latest' "
    echo "  -cow_port <port> : The port to use for the cow container. Default is 9998 "
}

# Function to handle cleanup when script is interrupted
cleanup() {
    echo "Stopping the Docker container co2..."
    docker stop co2 >/dev/null 2>&1
    docker rm co2 >/dev/null 2>&1
    echo "Stopping the Docker container cow..."
    docker stop cow >/dev/null 2>&1
    docker rm cow >/dev/null 2>&1
    echo "Script interrupted. Exiting..."
    exit 1
}

# Trap SIGINT (Ctrl+C) and call the cleanup function
trap cleanup SIGINT

# Check for optional parameter
co2_docker_image_tag="latest"
co2_port=9999
cow_docker_image_tag="latest"
cow_port=9998

# Parse command-line options
while [[ $# -gt 0 ]]; do
    case "$1" in
    -co2_tag)
        shift
        if [ $# -gt 0 ]; then
            co2_docker_image_tag=$1
        else
            display_usage
            exit 1
        fi
        ;;
    -co2_port)
        shift
        if [ $# -gt 0 ]; then
            port=$1
        else
            display_usage
            exit 1
        fi
        ;;
    -cow_tag)
        shift
        if [ $# -gt 0 ]; then
            cow_docker_image_tag=$1
        else
            display_usage
            exit 1
        fi
        ;;
    -cow_port)
        shift
        if [ $# -gt 0 ]; then
            cow_port=$1
        else
            display_usage
            exit 1
        fi
        ;;
    *)
        # Unknown option
        display_usage
        exit 1
        ;;
    esac
    shift
done


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


# update images
docker pull eu.gcr.io/finngen-sandbox-v3-containers/co2:$docker_image_tag
docker pull eu.gcr.io/finngen-sandbox-v3-containers/cow:$docker_image_tag


# create config files
echo "
urlCohortOperationsViewer: http://127.0.0.1:9998/?pathToResultsZip=
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

# Stop and remove the existing container, if it exists
echo "Stopping the existing Docker container co2..."
docker stop co2 >/dev/null 2>&1
docker rm co2 >/dev/null 2>&1
echo "Stopping the existing Docker container cow..."
docker stop cow >/dev/null 2>&1
docker rm cow >/dev/null 2>&1

docker run -d -p $co2_port:8888 -v /tmp:/tmp  \
    -e CO2_CONFIG_FILE="/tmp/co2_config.yml"  -e CO2_DATABASES_CONFIG_FILE="/tmp/co2_databases_config.yml" \
    --name co2  \
    eu.gcr.io/finngen-sandbox-v3-containers/co2:$co2_docker_image_tag

docker run -d -p $cow_port:8888 -v /tmp:/tmp  \
    --name cow  \
    eu.gcr.io/finngen-sandbox-v3-containers/cow:$cow_docker_image_tag

# Check if the container was started successfully
if [ $? -ne 0 ]; then
    echo "Failed to run the Docker containers."
    exit 1
fi

echo "Docker image pulled and container started successfully."

# Show container logs continuously
echo "Container logs:"
docker logs -f co2 &

# open the browser
firefox http://localhost:9999 &

# Wait indefinitely (or until interrupted) to keep the script running
echo "Press Ctrl+C to stop the Docker container and exit."
while true; do
    sleep 1
done

