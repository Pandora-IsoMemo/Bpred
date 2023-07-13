testthat::test_that("Test module remoteModels", {
  testApiOut <- getGithubContent(githubRepo = "bpred")
  
  testthat::expect_true(length(testApiOut) > 0)
  testthat::expect_true(length(getRemoteModelsFromGithub(githubRepo = "bpred",
                                                         apiOut = testApiOut)) > 0)
  testthat::expect_true(length(testApiOut) == 
                          length(getRemoteModelsFromGithub(githubRepo = "bpred",
                                                           apiOut = testApiOut)))
  
  testModel <- getRemoteModelsFromGithub(githubRepo = "bpred",
                                         apiOut = testApiOut)[1]
  
  testServer(remoteModelsServer,
             {
               # Arrange
               print("test empty data input")
               # Act
               session$setInputs(
                 remoteModelChoice = testModel,
                 loadRemoteModel = 1
               )
               
               testthat::expect_true(length(remoteChoices()) > 0)
               testthat::expect_true(testModel %in% remoteChoices())
               testthat::expect_equal(substr(pathToRemote(), start = 1, stop = 5), "/tmp/")
             })
})
