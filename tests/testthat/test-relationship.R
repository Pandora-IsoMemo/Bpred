testthat::test_that("computeResults simple example", {
  set.seed(100)
  #create simulated data set
  n <- 100
  y <- rnorm(n)
  x <- rnorm(n)
  y <- 1.5 + 2 * x + rnorm(n)
  xunc <- rep(0.25, length(x))
  yunc <- rep(0.25, length(y))
  xobs <- x + (rnorm(n, sd = xunc))
  yobs <- y + (rnorm(n, sd = yunc))
  form <- paste0("{slope} * [x] + {intercept}")
  parNames <- gsub("[\\{\\}]", "", regmatches(form, gregexpr("\\{.*?\\}", form))[[1]])
  varNames <- gsub("\\[|\\]", "", regmatches(form, gregexpr("\\[.*?\\]", form))[[1]])
  parNamesDir <- NULL
  
  
  f1 <-
    mpiBpred::fitModel(
      y = yobs,
      X = data.frame(x= xobs),
      yUnc = yunc,
      xUnc = data.frame(xunc= xunc),
      form = form,
      chains = 2,
      parNames = parNames, varNames = varNames,
      parNamesDir = parNamesDir,
      shinyUse = FALSE
    )
  
  testthat::expect_type(f1, "list")
  testthat::expect_length(f1, 11)
  testthat::expect_equal(mean(f1$beta[,2]), 1.51, tolerance = 0.01)
  
  form <- paste0("{slope} * [x]")
  parNames <- gsub("[\\{\\}]", "", regmatches(form, gregexpr("\\{.*?\\}", form))[[1]])
  varNames <- gsub("\\[|\\]", "", regmatches(form, gregexpr("\\[.*?\\]", form))[[1]])
  
  f2 <-
    suppressWarnings({mpiBpred::fitModel(
      y = yobs,
      X = data.frame(x= xobs),
      yUnc = yunc,
      xUnc = data.frame(xunc= xunc),
      form = form,
      chains = 2,
      parNames = parNames, varNames = varNames,
      parNamesDir = parNamesDir,
      shinyUse = FALSE
    )})
  testthat::expect_type(f2, "list")
  testthat::expect_length(f2, 11)
})

testthat::test_that("computeResults simple example wrong link negative data", {
  #create simulated data set
  n <- 100
  y <- rnorm(n)
  x <- rnorm(n)
  y <- 1.5 + 2 * x + rnorm(n)
  xunc <- rep(0.25, length(x))
  yunc <- rep(0.25, length(y))
  xobs <- x + (rnorm(n, sd = xunc))
  yobs <- y + (rnorm(n, sd = yunc))
  form <- paste0("{slope} * sqrt([x]) + {intercept}")
  parNames <- gsub("[\\{\\}]", "", regmatches(form, gregexpr("\\{.*?\\}", form))[[1]])
  varNames <- gsub("\\[|\\]", "", regmatches(form, gregexpr("\\[.*?\\]", form))[[1]])
  parNamesDir <- NULL
  
  f3 <- 
    suppressWarnings({mpiBpred::fitModel(
      y = yobs,
      X = data.frame(x= xobs),
      yUnc = yunc,
      xUnc = data.frame(xunc= xunc),
      form = form,
      chains = 2,
      parNames = parNames, varNames = varNames,
      parNamesDir = parNamesDir,
      shinyUse = FALSE
    )})
  testthat::expect_type(f3, "character")
})

testthat::test_that("test positive data example", {
  #create simulated data set
  set.seed(100)
  n <- 1000
  y <- rnorm(n)
  x <- exp(runif(n)*2+1)
  y <- -0.5 + 0.2 * log(x) + rnorm(n, sd = 0.25)
  xunc <- rep(0.1, length(x))
  yunc <- rep(0.1, length(y))
  xobs <- (x + (rnorm(n, sd = xunc)))
  yobs <- (y + (rnorm(n, sd = yunc)))
  form <- paste0("{slope} * log([x]) + {intercept}")
  parNames <- gsub("[\\{\\}]", "", regmatches(form, gregexpr("\\{.*?\\}", form))[[1]])
  varNames <- gsub("\\[|\\]", "", regmatches(form, gregexpr("\\[.*?\\]", form))[[1]])
  parNamesDir <- NULL
  
  f1 <-
    mpiBpred::fitModel(
      y = yobs,
      X = data.frame(x= xobs),
      yUnc = yunc,
      xUnc = data.frame(xunc= xunc),
      form = form,
      chains = 2,
      parNames = parNames, varNames = varNames,
      parNamesDir = parNamesDir,
      shinyUse = FALSE
    )
  
  testthat::expect_type(f1, "list")
  testthat::expect_length(f1, 11)
  testthat::expect_equal(mean(f1$beta[,1]), 0.2, tolerance = 0.03)
  ##
  form <- paste0("{slope} * sqrt([x]) + {intercept}")
  parNames <- gsub("[\\{\\}]", "", regmatches(form, gregexpr("\\{.*?\\}", form))[[1]])
  varNames <- gsub("\\[|\\]", "", regmatches(form, gregexpr("\\[.*?\\]", form))[[1]])
  parNamesDir <- NULL
  
  f2 <-
    mpiBpred::fitModel(
      y = yobs,
      X = data.frame(x= xobs),
      yUnc = yunc,
      xUnc = data.frame(xunc= xunc),
      form = form,
      chains = 2,
      parNames = parNames, varNames = varNames,
      parNamesDir = parNamesDir,
      shinyUse = FALSE
    )
  
  testthat::expect_type(f2, "list")
  testthat::expect_length(f2, 11)
  testthat::expect_equal(mean(f2$beta[,2]), -0.45, tolerance = 0.05)
})
