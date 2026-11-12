test_that("multiple cohort files are combined into standard cohortData", {
  files <- data.frame(
    datapath = c(
      testthat::test_path("testdata", "cohortDataFiles", "person_source_values.tsv"),
      testthat::test_path("testdata", "cohortDataFiles", "eunomia_nostartdate.csv")
    ),
    name = c("person_source_values.tsv", "eunomia_nostartdate.csv"),
    stringsAsFactors = FALSE
  )

  combined <- CohortOperations2:::.read_multiple_cohort_files(files)

  expect_named(
    combined,
    c("cohort_name", "person_source_value", "cohort_start_date", "cohort_end_date")
  )
  expect_setequal(
    unique(combined$cohort_name),
    c("person_source_values", "Cohort A", "Cohort B")
  )
  expect_true(all(nchar(combined$person_source_value) > 0))
})

test_that("multiple cohort files reject incompatible schemas", {
  incompatible_path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("foo,bar", "1,2"), incompatible_path)
  files <- data.frame(
    datapath = incompatible_path,
    name = "incompatible.csv",
    stringsAsFactors = FALSE
  )

  expect_error(
    CohortOperations2:::.read_multiple_cohort_files(rbind(files, files)),
    "Multiple-file import requires compatible files"
  )
})

test_that("incompatible files can be returned for manual mapping", {
  unmapped_path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("group,subject,visit_date,extra", "A,person-1,2020-01-01,x"), unmapped_path)
  files <- data.frame(
    datapath = unmapped_path,
    name = "unmapped.csv",
    stringsAsFactors = FALSE
  )

  result <- CohortOperations2:::.read_multiple_cohort_files(
    files,
    allow_unmapped = TRUE
  )

  expect_length(result$data, 0)
  expect_length(result$unmapped, 1)
  expect_identical(result$unmapped[[1]]$name, "unmapped.csv")
  expect_named(result$unmapped[[1]]$data, c("group", "subject", "visit_date", "extra"))
})
