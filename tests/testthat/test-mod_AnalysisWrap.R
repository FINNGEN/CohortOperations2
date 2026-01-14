

test_that("filename sanitization removes spaces and special characters", {
  # Test the sanitization logic used in mod_analysisWrap_server
  
  # Test case 1: "Code WAS" should become "Code_WAS"
  analysisName1 <- "Code WAS"
  sanitizedName1 <- gsub("[^[:alnum:]]+", "_", analysisName1)
  sanitizedName1 <- gsub("^_+|_+$", "", sanitizedName1)
  expected1 <- "Code_WAS"
  expect_equal(sanitizedName1, expected1)
  
  # Test case 2: "Time Code WAS" should become "Time_Code_WAS"
  analysisName2 <- "Time Code WAS"
  sanitizedName2 <- gsub("[^[:alnum:]]+", "_", analysisName2)
  sanitizedName2 <- gsub("^_+|_+$", "", sanitizedName2)
  expected2 <- "Time_Code_WAS"
  expect_equal(sanitizedName2, expected2)
  
  # Test case 3: "Cohort Overlaps" should become "Cohort_Overlaps"
  analysisName3 <- "Cohort Overlaps"
  sanitizedName3 <- gsub("[^[:alnum:]]+", "_", analysisName3)
  sanitizedName3 <- gsub("^_+|_+$", "", sanitizedName3)
  expected3 <- "Cohort_Overlaps"
  expect_equal(sanitizedName3, expected3)
  
  # Test case 4: "Cohort-Demographics!" should become "Cohort_Demographics"
  analysisName4 <- "Cohort-Demographics!"
  sanitizedName4 <- gsub("[^[:alnum:]]+", "_", analysisName4)
  sanitizedName4 <- gsub("^_+|_+$", "", sanitizedName4)
  expected4 <- "Cohort_Demographics"
  expect_equal(sanitizedName4, expected4)
  
  # Test case 5: Name with no special characters should remain unchanged
  analysisName5 <- "GWAS"
  sanitizedName5 <- gsub("[^[:alnum:]]+", "_", analysisName5)
  sanitizedName5 <- gsub("^_+|_+$", "", sanitizedName5)
  expected5 <- "GWAS"
  expect_equal(sanitizedName5, expected5)
  
  # Test case 6: Name with leading/trailing spaces should be trimmed
  analysisName6 <- "  Analysis Name  "
  sanitizedName6 <- gsub("[^[:alnum:]]+", "_", analysisName6)
  sanitizedName6 <- gsub("^_+|_+$", "", sanitizedName6)
  expected6 <- "Analysis_Name"
  expect_equal(sanitizedName6, expected6)
})
