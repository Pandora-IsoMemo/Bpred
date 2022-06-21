context("Plot estimates")

test_that("plots creation", {
  load(testthat::test_path("testdata/test_plots.RData"))
  
  g <- plotDensities(yEstimates, type = "Category", plotType = "KernelDensity")
  
  
  expect_type(g, "list")
  expect_length(g, 9)
  expect_equal(class(g), c("gg", "ggplot"))
})
