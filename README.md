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

A demo app can be run using the following Docker project:

[FINNGEN/CO2DockerDemo](https://github.com/FINNGEN/CO2DockerDemo)

Alternatively, it can be run locally using the following R commands:

Install the package and dependencies:
``` r
install.packages("remotes")
remotes::install_github("FINNGEN/CohortOperations2")
# necessary to run the demo with the CO2AnalysisModules
remotes::install_github("FINNGEN/CO2AnalysisModules")
```

Run the demo:
``` r
pathToCohortOperationsConfigYaml <- testthat::test_path("config", "devatlas_databasesConfig.yml")
pathToDatabasesConfigYaml <- testthat::test_path("config", "test_analysisModulesConfig.yml")  
    
CohortOperations2::runApp(
    pathToCohortOperationsConfigYaml = pathToCohortOperationsConfigYaml,
    pathToDatabasesConfigYaml = pathToDatabasesConfigYaml,
    options = list(port = 9999, launch.browser = TRUE)
)
```

This demo is configured to run using the Eunomia databases, GiBleed and MIMIC, connect to the [public ATLAS instance](https://atlas-demo.ohdsi.org/) and include all the analyses from the [CO2AnalysisModules package](https://github.com/FINNGEN/CO2AnalysisModules).

## Customization

A custom app can be configured to connect one or more OMOP CDM compliant databases, and an associated ATLAS instance. An example configuration of the databases and ATLAS is stored in [`tests/testthat/config/devatlas_databasesConfig.yml`](tests/testthat/config/devatlas_databasesConfig.yml).

A custom app can be configured to show different analyses modules. An example configuration of the analyses modules is stored in [`tests/testthat/config/test_analysisModulesConfig.yml`](tests/testthat/config/test_analysisModulesConfig.yml).


