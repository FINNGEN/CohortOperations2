# CohortOperations2

<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

CohortOperations2 is a Shiny app that allows to import cohorts from multiple sources, combine them into new cohorts and use them to execute analyses. 

The base app connects to an OMOP CDM database, creates a cohort_table and allows to :

* Import cohorts from an ATLAS instance
* Import cohorts from a text file
* Import cohorts from a precalculated library of cohorts
* Create new cohorts by applying set operations to existing ones
* Create new cohorts by matching age, sex and index date to a target cohort
* Export cohorts to a text file
* Select cohorts and other parameters to execute analyses

Analyses are build as modules. The list of possible analyses can be customized according to the needs of the project, or new modules can added. 
Sources for analyses modules can be found in:
* [FINNGEN/CO2AnalysisModules](https://github.com/FINNGEN/CO2AnalysisModules)



## Demo

A demo configuration is provided using Eunomia databases (GiBleed and MIMIC) and the [public ATLAS instance](https://atlas-demo.ohdsi.org/).

Demo can be run using Docker, which is more convenient, or R.

[See video demo](https://www.ohdsi.org/2024showcase-131/)

### Using Docker 

A demo app can be run using the following Docker project:

[FINNGEN/CO2DockerDemo](https://github.com/FINNGEN/CO2DockerDemo)

### Using R

Alternatively, it can be run locally using the following R commands:

Install the package and dependencies:
``` r
install.packages("renv")
renv::init()
renv::install("FINNGEN/CohortOperations2", prompt = FALSE, lock = TRUE)
renv::install("FINNGEN/CO2AnalysisModules", prompt = FALSE, lock = TRUE)
# TEMPORARY FIX, atm it needs some specific versions of dbplyr and FeatureExtraction
renv::install("dbplyr@2.4.0", prompt = FALSE, lock = TRUE)
renv::install("FeatureExtraction@3.6.0", prompt = FALSE, lock = TRUE)
```

Get the config files from github:
``` bash
wget https://raw.githubusercontent.com/FINNGEN/CohortOperations2/master/tests/testthat/config/eunomia_databasesConfig.yml
wget https://raw.githubusercontent.com/FINNGEN/CohortOperations2/master/tests/testthat/config/test_analysisModulesConfig.yml
```

Prepare config:
``` r
pathToGiBleedEunomiaSqlite <- Eunomia::getDatabaseFile("GiBleed")
pathToMIMICEunomiaSqlite <- Eunomia::getDatabaseFile("MIMIC")

databasesConfig <- CohortOperations2::readAndParseYalm(
    pathToYalmFile =  "eunomia_databasesConfig.yml",
    pathToGiBleedEunomiaSqlite = pathToGiBleedEunomiaSqlite,
    pathToMIMICEunomiaSqlite = pathToMIMICEunomiaSqlite
)

analysisModulesConfig <- CohortOperations2::readAndParseYalm(
    pathToYalmFile =  "test_analysisModulesConfig.yml"
)
```

Run the app:
``` r
CohortOperations2::run_app(
  databasesConfig = databasesConfig,
  analysisModulesConfig = analysisModulesConfig,
  options = list(port = 9999, launch.browser = TRUE)
)
```

This allows to run the CohortOperations2 app. However, to open the viewers, the corresponding viewer apps need to be running in the port stated in the analysisModulesConfig.yml file. For example using an other Rstudio


## Customization

A custom app can be configured to connect one or more OMOP CDM compliant databases, and an associated ATLAS instance. An example configuration of the databases and ATLAS is stored in [`tests/testthat/config/devatlas_databasesConfig.yml`](tests/testthat/config/devatlas_databasesConfig.yml).

A custom app can be configured to show different analyses modules. An example configuration of the analyses modules is stored in [`tests/testthat/config/test_analysisModulesConfig.yml`](tests/testthat/config/test_analysisModulesConfig.yml).


