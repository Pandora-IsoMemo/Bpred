context("Plot estimates")

testthat::test_that("plots creation", {
  load(testthat::test_path("testdata/test_plots.RData"))
  
  g <- mpiBpred::plotDensities(yEstimates, type = "Category", plotType = "KernelDensity")
  
  
  testthat::expect_type(g, "list")
  testthat::expect_length(g, 9)
  testthat::expect_equal(class(g), c("gg", "ggplot"))
})
