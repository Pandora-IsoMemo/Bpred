testthat::test_that("plots creation", {
  yEstimates <- readRDS(testthat::test_path("testdata/yEstimates.rds"))
  
  g <-
    Bpred::plotDensities(yEstimates, type = "Category", plotType = "KernelDensity")
  
  
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

testthat::test_that("plot of formulas", {
  yEstimates <- readRDS(testthat::test_path("testdata/yEstimates.rds"))
  
  g <-
    plotFunctions(
      data = structure(
        c(0.295, -0.692, -1.528, -0.757, -2.612, -1.48, 0.576, 
          0.286, 0.239, -0.091, 2.338, -0.833, -0.835, 0.347, -3.58, -2.132, 
          2.91, 2.91, 2.631, 2.184, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 
          0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 
          0.25, 0.25, 0.25, 0.412, -0.756, -1.241, -0.659, -2.817, -1.481, 
          0.615, -0.093, 0.622, 0.438, 2.28, -0.855, -0.833, 0.138, -3.959, 
          -2.204, 2.948, 3.377, 2.551, 2.247, 3.254, 0.968, 1.985, 3.135, 
          2.547, 2.402, 4.141, 2.924, 2.02, 2.597, 2.294, -0.871, -0.793, 
          -0.121, -3.461, -1.926, 3.041, 2.878, 2.663, 2.624, 2.121, 1.486, 
          -0.317, -1.087, 0.215, 2.349, 2.151, 0.266, -0.796, -0.599), 
        dim = 10:9,
        dimnames = list(NULL, 
                        c("x", "y", "xunc", "yunc", "xobs", "yobs", "y2", "yobs2", "x2"))
      ), 
      xVar = "x",
      yVar = "y",
      obj = yEstimates$regfunctions$formula_1,
      PointSize = 1, 
      LineWidth = 1,
      prop = 0.9,
      alpha = 0.4)$g
  
  
  testthat::expect_type(g, "list")
  testthat::expect_equal(g$labels, list(x = "x", y = "y", ymin = "yMin", ymax = "yMax"))
  testthat::expect_equal(class(g), c("gg", "ggplot"))
})
