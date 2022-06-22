context("estimate posterior")

test_that("posterior estimation simple", {
  load(testthat::test_path("testdata/test_post.RData"))
  set.seed(1000)
  data <- data.frame(Category = c("Site1", "Site1", "Site1", "Site2", "Site2"),
                     X1 = c(1, 0.9, 1.2, 4, 5),
                     SD_X1 = c(0.2, 0.3, 0.2, 0.2, 0.3),
                     X2 = c(1.5, 1.8, 1.1, 2.25, 2.3),
                     SD_X2 = c(0.5, 0.3, 0.2, 0.2, 0.3))
  
  yEstimates <- estimateY(relationship = "Y ~ 3 + 1 * one(X1) + two(X2) / 2",
                          regfunctions = list(one = f1, two = f2),
                          indVars = c("X1", "X2"),
                          data = data,
                          indVarsUnc = c("SD_X1", "SD_X2"),
                          category = "Category",
                          n_samples = 1000,
                          includeRegUnc = TRUE)
  expect_type(yEstimates, "list")
  expect_length(yEstimates, 3)
  expect_length(yEstimates$Y_Samples_Individual, 5)
  expect_length(yEstimates$Y_Samples_Category, 2)
  expect_equal(mean(yEstimates$Y_Samples_Combined), 7.265, tolerance = 0.01)
  expect_equal(mean(yEstimates$Y_Samples_Combined), mean(unlist(yEstimates$Y_Samples_Individual)))
  expect_equal(mean(yEstimates$Y_Samples_Combined), mean(unlist(yEstimates$Y_Samples_Category)))
  
  expect_error(estimateY(relationship = "Y ~ 3 + 1 * f3(X2 * f1(X1))",
                          regfunctions = list(f1 = f1, f2 = f2),
                          indVars = c("X1", "X2"),
                          data = data,
                          indVarsUnc = c("SD_X1", "SD_X2"),
                          n_samples = 1000,
                          includeRegUnc = TRUE))
})

test_that("posterior estimation complex", {
  load(testthat::test_path("testdata/test_post.RData"))
  set.seed(1000)
  data <- data.frame(Category = c("Site1", "Site1", "Site1", "Site2", "Site2"),
                     X1 = c(1, 0.9, 1.2, 4, 5),
                     SD_X1 = c(0.2, 0.3, 0.2, 0.2, 0.3),
                     X2 = c(1.5, 1.8, 1.1, 2.25, 2.3),
                     SD_X2 = c(0.5, 0.3, 0.2, 0.2, 0.3))
  
  yEstimates <- estimateY(relationship = "Y ~ 3 + 1 * two(X2 * one(X1))",
                          regfunctions = list(one = f1, two = f2),
                          indVars = c("X1", "X2"),
                          data = data,
                          indVarsUnc = c("SD_X1", "SD_X2"),
                          n_samples = 1000,
                          includeRegUnc = TRUE)
  expect_type(yEstimates, "list")
  expect_length(yEstimates, 2)
  expect_length(yEstimates$Y_Samples_Individual, 5)
  expect_equal(mean(yEstimates$Y_Samples_Combined), 6.097, tolerance = 0.01)
  expect_equal(mean(yEstimates$Y_Samples_Combined), mean(unlist(yEstimates$Y_Samples_Individual)))
})

test_that("posterior estimation no uncertainty", {
  load(testthat::test_path("testdata/test_post.RData"))
  set.seed(1000)
  data <- data.frame(Category = c("Site1", "Site1", "Site1", "Site2", "Site2"),
                     X1 = c(1, 0.9, 1.2, 4, 5),
                     SD_X1 = c(0.2, 0.3, 0.2, 0.2, 0.3),
                     X2 = c(1.5, 1.8, 1.1, 2.25, 2.3),
                     SD_X2 = c(0.5, 0.3, 0.2, 0.2, 0.3))
  
  yEstimates <- estimateY(relationship = "Y ~ 3 + 1 * two(X2 * one(X1))",
                          regfunctions = list(one = f1, two = f2),
                          indVars = c("X1", "X2"),
                          data = data,
                          n_samples = 1000,
                          includeRegUnc = TRUE)
  expect_type(yEstimates, "list")
  expect_length(yEstimates, 2)
  expect_length(yEstimates$Y_Samples_Individual, 5)
  expect_equal(mean(yEstimates$Y_Samples_Combined), 6.18, tolerance = 0.01)
  expect_equal(mean(yEstimates$Y_Samples_Combined), mean(unlist(yEstimates$Y_Samples_Individual)))
})
