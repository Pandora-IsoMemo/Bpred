testthat::test_that("Summarise estimates table", {
  yEstimates <- readRDS(testthat::test_path("testdata/yEstimates.rds"))
  
  result <-
    Bpred::summariseEstimates(
      yEstimates,
      type = "Sample",
      probability = 0.95,
      checkDifferencesReference = TRUE,
      referenceType = "dist",
      referenceDist = "norm",
      referenceParameters = c(50, 3)
    )
  
  testthat::expect_type(result, "list")
  testthat::expect_length(result, 2)
  testthat::expect_equal(nrow(result$Means), 6)
  
  result <-
    Bpred::summariseEstimates(yEstimates, type = "Sample", probability = 0.95)
  
  testthat::expect_type(result, "list")
  testthat::expect_length(result, 2)
  testthat::expect_equal(nrow(result$Means), 5)
  
  result <-
    Bpred::summariseEstimates(yEstimates, type = "Combined", probability = 0.95)
  
  testthat::expect_type(result, "list")
  testthat::expect_length(result, 1)
  testthat::expect_equal(nrow(result$Means), 1)
  
  result <-
    Bpred::summariseEstimates(yEstimates, type = "Category", probability = 0.95)
  
  testthat::expect_type(result, "list")
  testthat::expect_length(result, 2)
  testthat::expect_equal(nrow(result$Means), 2)
  
  result <-
    Bpred::summariseEstimates(
      yEstimates,
      type = "Sample",
      probability = 0.95,
      checkDifferencesReference = TRUE,
      referenceType = "dist",
      referenceDist = "norm",
      referenceParameters = c(40, 3)
    )
  
  testthat::expect_type(result, "list")
  testthat::expect_length(result, 2)
  
  result <-
    Bpred::summariseEstimates(
      yEstimates,
      type = "Sample",
      probability = 0.95,
      checkDifferencesReference = TRUE,
      referenceType = "sample",
      referenceSample = c(60, 52, 75, 48, 50, 56)
    )
  
  testthat::expect_type(result, "list")
  testthat::expect_length(result, 2)
})
