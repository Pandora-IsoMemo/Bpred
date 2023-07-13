testthat::test_that("enrichForm", {
  testFormDF <-
    structure(
      list(
        name = "formula_1",
        y = "y",
        x = "",
        yUnc = "none",
        xUnc = "none",
        link = "none",
        formula = "{slope} * [x] + {intercept}"
      ),
      class = "data.frame",
      row.names = c(NA, -1L)
    )
  
  testFormulas <- readRDS(file.path(testthat::test_path("test-formulas_data.rds")))
  
  testY <- c(0.731, -2.593, 4.93, -0.207, 3.008, 2.025, 5.122, -0.771, 7.548, 
             -3.588, 2.989, -1.177, -0.959, 1.629, 4.362, -2.415, -2.49, 5.34, 
             3.053, 1.577, 3.899, -2.667, 1.447, 2.177, 4.139, -0.981, -1.098, 
             0.274, 0.955, -2.324, 2.4, 2.018, 3.253, 2.939, 1.099, 0.511, 
             -3.813, 0.41, 0.551, -0.206, 1.304, 4.67, -2.114, 0.675, -1.846, 
             -1.77, 3.482, 1.796, -0.707, 0.728, 1.548, 0.605, 0.816, 2.859, 
             2.777, -0.321, 2.447, 2.381, -0.264, 3.465, 2.268, 0.5, 1.462, 
             1.997, 4.161, 2.335, 1.067, 2.486, 5.132, -3.345, 1.654, 0.345, 
             1.849, 0.38, 0.756, 7.813, 4.178, 2.922, -0.666, 1.947, 0.068, 
             1.886, 0.002, 2.394, 1.349, 0.706, 2.429, 1.178, 1.047, 0.052, 
             -1.811, 4.458, -0.532, 7.849, 2.103, 3.085, -0.395, 2.766, 2.264, 
             0.015)
  
  testFormRes <- testFormDF %>% enrichForm(parNames = c("slope", "intercept"),
                                           formulasObject = testFormulas, 
                                           y = testY)
  
  formExpected <-
    structure(
      list(
        name = "formula_1",
        y = "y",
        x = "",
        yUnc = "none",
        xUnc = "none",
        link = "none",
        formula = "{slope} * [x] + {intercept}",
        parameter = "slope = 2.026, intercept = 1.537",
        credible_intervals_95p = "slope = (1.824,2.221), intercept = (1.342,1.726)",
        R_squared = 0.8164,
        p_direction = "slope = (1), intercept = (1)"
      ),
      class = "data.frame",
      row.names = c(NA, -1L)
    )
  
  testthat::expect_equal(testFormRes, formExpected)
})