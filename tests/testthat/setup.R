# settings
testConfigFile <- "eunomia_databasesConfig.yml"

databasesConfig <- yaml::read_yaml(testthat::test_path("config", testConfigFile))

message("************* Testing on ", testConfigFile, " *************")
