

# Running

### Run in development


To run the application in development mode, you can use the following command:

```bash

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
" > /tmp/co2_config.yml

docker run -p 8888:8888 -v /tmp:/tmp  eu.gcr.io/finngen-sandbox-v3-containers/co2:0.0.1

```



