# CohortOperations2

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of CohortOperations2 is to ...

## Installation

Clone repository, open project in RStudio and run 

``` r
renv::restore()
```

## Run

Run  app in development Using Eunomia

```R
devtools::load_all(".");

run_app(
  pathToCohortOperationsConfigYalm = testthat::test_path("config", "cohortOperationsConfig.yml"),
  pathToDatabasesConfigYalm = testthat::test_path("config", "eunomia_databasesConfig.yml"),
  options = list(port = 9999, launch.browser = TRUE)
  )
```

Run modules in development 

Modules can be developed independently using the corresponding files in `test/testmanual/test-<module_name>`. 

Each of these files has two parts. The first, generates all the inputs needed for the module with dummy data. The second, runs the module as a Shiny app. 
This way is not necessary to lunch the full app and click all the way to get the initial state of the module. 

Manual testing uses the Eunomia database for speed. However, that can be changed in `test/testthat/setup.R` following standart `testthat` practices. 
