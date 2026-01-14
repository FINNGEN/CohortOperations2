test_that("filename sanitization removes spaces and special characters", {
  # Helper function that matches the sanitization logic used in mod_analysisWrap_server
  sanitize_filename <- function(name) {
    sanitized <- gsub("[^[:alnum:]]+", "_", name)
    gsub("^_+|_+$", "", sanitized)
  }
  
  # Test case 1: "Code WAS" should become "Code_WAS"
  expect_equal(sanitize_filename("Code WAS"), "Code_WAS")
  
  # Test case 2: "Time Code WAS" should become "Time_Code_WAS"
  expect_equal(sanitize_filename("Time Code WAS"), "Time_Code_WAS")
  
  # Test case 3: "Cohort Overlaps" should become "Cohort_Overlaps"
  expect_equal(sanitize_filename("Cohort Overlaps"), "Cohort_Overlaps")
  
  # Test case 4: "Cohort-Demographics!" should become "Cohort_Demographics"
  expect_equal(sanitize_filename("Cohort-Demographics!"), "Cohort_Demographics")
  
  # Test case 5: Name with no special characters should remain unchanged
  expect_equal(sanitize_filename("GWAS"), "GWAS")
  
  # Test case 6: Name with leading/trailing spaces should be trimmed
  expect_equal(sanitize_filename("  Analysis Name  "), "Analysis_Name")
})
