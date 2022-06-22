test_that("Test module exportPopUp", {
  testServer(exportPopUpServer, args = NULL,
             {
               # Arrange
               print("test empty data input")
               # Act
               session$setInputs(
                 exportType = "xlsx",
                 colseparator = ",",
                 decseparator = ".",
                 exportExecute = TRUE
               )
               
               testthat::expect_error(output$exportExecute)
             })
  
  testData <-
    list(Means = structure(
      list(
        Individual = c("1", "2", "3", "4",
                       "5"),
        Mean = c(18.262, 17.493, 19.803, 41.265, 48.814),
        Sd = c(1.623,
               2.354, 1.627, 2.213, 3.019),
        Int_Lower = c(15.14, 12.989, 16.691,
                      37.13, 42.976),
        Int_Upper = c(21.476, 22.198, 23.104, 45.809,
                      54.839)
      ),
      class = "data.frame",
      row.names = c("1", "2", "3",
                    "4", "5")
    ))
  
  testServer(exportPopUpServer, args = list(exportData = reactive(testData),
                                            filename = "Summary"),
             {
               # Arrange
               print("test export popUp")
               # Act
               session$setInputs(
                 exportType = "xlsx",
                 colseparator = ",",
                 decseparator = ".",
                 exportExecute = TRUE
               )
               
               testthat::expect_true(grepl("Summary.xlsx", output$exportExecute))
             })
})


test_that("Test module exportPlotPopUp", {
  testServer(exportPlotPopUpServer, args = NULL,
             {
               # Arrange
               print("test empty plot input")
               # Act
               session$setInputs(
                 exportType = "png",
                 width = 1280,
                 height = 800,
                 exportExecute = TRUE
               )
               testthat::expect_error(output$exportExecute)
             })
  
  testPlot <- readRDS(testthat::test_path("plot-module-exportPopUp.rds"))
  
  testServer(exportPlotPopUpServer, args = list(exportPlot = reactive(testPlot),
                                            filename = "plotEstimates"),
             {
               # Arrange
               print("test export popUp")
               # Act
               session$setInputs(
                 exportType = "png",
                 width = 1280,
                 height = 800,
                 exportExecute = TRUE
               )
               
               testthat::expect_length(output$preview, 5)
               testthat::expect_equal(names(output$preview), 
                                      c("src", "width", "height", "alt", "coordmap"))
               testthat::expect_equal(names(output$preview$width), 600)
               testthat::expect_equal(names(output$preview$height), 400)
               testthat::expect_true(grepl("plotEstimates.png", output$exportExecute))
             })
})
