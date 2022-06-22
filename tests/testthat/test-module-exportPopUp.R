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
                 exportExecuteSummary = TRUE
               )
               
               expect_error(output$exportExecuteSummary)
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
  
  testServer(exportPopUpServer, args = list(exportData = reactive(testData)),
             {
               # Arrange
               print("test export popUp")
               # Act
               session$setInputs(
                 exportType = "xlsx",
                 colseparator = ",",
                 decseparator = ".",
                 exportExecuteSummary = TRUE
               )
               
               expect_true(grepl("Summary.xlsx", output$exportExecuteSummary))
             })
})
