

# Running

## Run docker in sandbox

Replace the sandbox number with your sandbox number (e.g. fg-production-sandbox-6). 
Replace the table name with your table name (e.g. javier_test_cohort_table).
Replace the docker image version with the version you want to use (e.g. 0.0.1).

```bash
docker pull eu.gcr.io/finngen-sandbox-v3-containers/co2:0.0.1

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
        connectionString: jdbc:bigquery://https://www.googleapis.com/auth/bigquery:433;ProjectId=fg-production-sandbox-6;OAuthType=3;Timeout=10000;
        pathToDriver: /root/jdbc_drivers/bigquery 
      tempEmulationSchema: fg-production-sandbox-6.sandbox #needed for creating tmp table in BigQuery
      useBigrqueryUpload: true # option for HadesExtras
    cdm:
      cdmDatabaseSchema: finngen-production-library.finngen_omop_r12
      vocabularyDatabaseSchema: finngen-production-library.finngen_omop_r12
    cohortTable:
      cohortDatabaseSchema: fg-production-sandbox-6.sandbox
      cohortTableName: javier_test_cohort_table

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
        connectionString: jdbc:bigquery://https://www.googleapis.com/auth/bigquery:433;ProjectId=fg-production-sandbox-6;OAuthType=3;Timeout=10000;
        pathToDriver: /root/jdbc_drivers/bigquery 
      tempEmulationSchema: fg-production-sandbox-6.sandbox #needed for creating tmp table in BigQuery
      useBigrqueryUpload: true # option for HadesExtras
    cdm:
      cdmDatabaseSchema: finngen-production-library.finngen_omop_r11
      vocabularyDatabaseSchema: finngen-production-library.finngen_omop_r11
    cohortTable:
      cohortDatabaseSchema: fg-production-sandbox-6.sandbox
      cohortTableName: javier_test_cohort_table

" > /tmp/co2_databases_config.yml

docker run -p 9999:8888 -v /tmp:/tmp  \
    -e CO2_CONFIG_FILE="/tmp/co2_config.yml"  -e CO2_DATABASES_CONFIG_FILE="/tmp/co2_databases_config.yml" \
    eu.gcr.io/finngen-sandbox-v3-containers/co2:0.0.1

```

Open the browser and go to http://localhost:9999/


### Run Docker image locally using Eunomia
 
Replace the docker image version with the version you want to use (e.g. 0.0.1).


```bash
cp /Users/javier/keys/atlas-development-270609-410deaacc58b.json /tmp/

echo "
urlCohortOperationsViewer: http://127.0.0.1:9999/?pathToResultsZip=
" > /tmp/co2_config.yml

echo "
# EUNOMIA
E1:
  cohortTableHandler:
    database:
      databaseId: E1
      databaseName: eunomia1
      databaseDescription: Eunomia database 1
    connection:
      connectionDetailsSettings:
          dbms: eunomia
    cdm:
        cdmDatabaseSchema: main
        vocabularyDatabaseSchema: main
    cohortTable:
        cohortDatabaseSchema: main
        cohortTableName: test_cohort_table
#  atlasConfig:
#     webapi:
#     resultsshchema:

# EUNOMIA 2
E2:
  cohortTableHandler:
    database:
      databaseId: E2
      databaseName: eunomia2
      databaseDescription: Eunomia database 2
    connection:
      connectionDetailsSettings:
          dbms: eunomia
    cdm:
        cdmDatabaseSchema: main
        vocabularyDatabaseSchema: main
    cohortTable:
        cohortDatabaseSchema: main
        cohortTableName: test_cohort_table
" > /tmp/co2_databases_config.yml

docker run -p 9999:8888 -v /tmp:/tmp  \
    -e CO2_CONFIG_FILE="/tmp/co2_config.yml"  -e CO2_DATABASES_CONFIG_FILE="/tmp/co2_databases_config.yml" \
    eu.gcr.io/finngen-sandbox-v3-containers/co2:0.0.1

```
 

Open the browser and go to http://localhost:9999/


### Run Docker image locally using AtlasDev with bigquery

Replace the path to the key with the path to your key (e.g. /Users/javier/keys/atlas-development-270609-410deaacc58b.json).
Replace the docker image version with the version you want to use (e.g. 0.0.1).

```bash
cp /Users/javier/keys/atlas-development-270609-410deaacc58b.json /tmp/

echo "
urlCohortOperationsViewer: http://127.0.0.1:9999/?pathToResultsZip=
" > /tmp/co2_config.yml

echo "
# EUNOMIA
E1:
  cohortTableHandler:
    database:
      databaseId: E1
      databaseName: eunomia1
      databaseDescription: Eunomia database 1
    connection:
      connectionDetailsSettings:
          dbms: eunomia
    cdm:
        cdmDatabaseSchema: main
        vocabularyDatabaseSchema: main
    cohortTable:
        cohortDatabaseSchema: main
        cohortTableName: test_cohort_table
#  atlasConfig:
#     webapi:
#     resultsshchema:

# EUNOMIA 2
BQ1:
  cohortTableHandler:
    database:
      databaseId: BQ1
      databaseName: bigquery1
      databaseDescription: BigQuery database
    connection:
      connectionDetailsSettings:
        dbms: bigquery
        user: ''
        password: ''
        connectionString: jdbc:bigquery://https://www.googleapis.com/bigquery/v2:443;ProjectId=atlas-development-270609;OAuthType=0;OAuthServiceAcctEmail=146473670970-compute@developer.gserviceaccount.com;OAuthPvtKeyPath=/tmp/atlas-development-270609-410deaacc58b.json;Timeout=100000;
        pathToDriver: /root/jdbc_drivers/bigquery 
      tempEmulationSchema: atlas-development-270609.sandbox #optional
      useBigrqueryUpload: true #optional
    cdm:
      cdmDatabaseSchema: atlas-development-270609.finngen_omop_r11
      vocabularyDatabaseSchema: atlas-development-270609.finngen_omop_r11
    cohortTable:
      cohortDatabaseSchema: atlas-development-270609.sandbox
      cohortTableName: test_cohort_table
" > /tmp/co2_databases_config.yml

docker run -p 9999:8888 -v /tmp:/tmp  \
    -e CO2_CONFIG_FILE="/tmp/co2_config.yml"  -e CO2_DATABASES_CONFIG_FILE="/tmp/co2_databases_config.yml" \
    eu.gcr.io/finngen-sandbox-v3-containers/co2:0.0.1

```

Open the browser and go to http://localhost:9999/


## Run the docker as rstudio server for debugging 

```bash
docker run -p 8787:8787 -v /tmp:/tmp  \
    -e PASSWORD=pass --entrypoint /init \
    eu.gcr.io/finngen-sandbox-v3-containers/co2:0.0.1
```

Open the browser and go to http://localhost:8787/