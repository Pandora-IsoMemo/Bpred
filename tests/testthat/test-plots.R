testthat::test_that("plots creation", {
  load(testthat::test_path("testdata/test_plots.RData"))
  
  g <-
    mpiBpred::plotDensities(yEstimates, type = "Category", plotType = "KernelDensity")
  
  
  testthat::expect_type(g, "list")
  testthat::expect_equal(
    g$labels,
    list(
      title = "Density by category",
      x = "Value",
      fill = "Category",
      y = structure("density", fallback = TRUE),
      weight = structure("weight", fallback = TRUE)
    )
  )
  testthat::expect_equal(class(g), c("gg", "ggplot"))
})
