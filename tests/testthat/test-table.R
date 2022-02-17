context("Summarise estimates")

test_that("Summarise estimates table", {
  load("testdata/test_plots.RData")
  
  result <- summariseEstimates(yEstimates, type = "Individual", probability = 0.95,
                               checkDifferencesReference = TRUE,
                               referenceType = "dist",
                               referenceDist = "norm", referenceParameters = c(50, 3))

  expect_type(result, "list")
  expect_length(result, 2)
  expect_equal(nrow(result$Means), 6)
  
  result <- summariseEstimates(yEstimates, type = "Individual", probability = 0.95)
  
  expect_type(result, "list")
  expect_length(result, 1)
  expect_equal(nrow(result$Means), 5)
  
  result <- summariseEstimates(yEstimates, type = "Combined", probability = 0.95)
  
  expect_type(result, "list")
  expect_length(result, 1)
  expect_equal(nrow(result$Means), 1)
  
  result <- summariseEstimates(yEstimates, type = "Category", probability = 0.95)
  
  expect_type(result, "list")
  expect_length(result, 1)
  expect_equal(nrow(result$Means), 2)

  result <- summariseEstimates(yEstimates, type = "Individual", probability = 0.95,
                               checkDifferencesReference = TRUE,
                               referenceType = "dist",
                               referenceDist = "norm", referenceParameters = c(40, 3))
  
  expect_type(result, "list")
  expect_length(result, 1)
  
  result <- summariseEstimates(yEstimates, type = "Individual", probability = 0.95,
                               checkDifferencesReference = TRUE,
                               referenceType = "sample",
                               referenceSample = c(60, 52, 75, 48, 50, 56))
  
  expect_type(result, "list")
  expect_length(result, 2)
})
