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
  
  f1 <-
    mpiBpred::estimateRelationship(
      y = yobs,
      x = xobs,
      yunc = yunc,
      xunc = xunc,
      link = "linIntcp",
      chains = 2
    )
  
  testthat::expect_type(f1, "list")
  testthat::expect_length(f1, 5)
  testthat::expect_equal(f1$type, 1)
  testthat::expect_equal(mean(f1$slope), 1.51, tolerance = 0.01)
  
  f2 <-
    mpiBpred::estimateRelationship(
      y = yobs,
      x = xobs,
      yunc = yunc,
      xunc = xunc,
      link = "linNoIntcp",
      chains = 2
    )
  testthat::expect_type(f2, "list")
  testthat::expect_length(f2, 5)
  testthat::expect_equal(f2$type, 2)
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
  testthat::expect_error(
    mpiBpred::estimateRelationship(
      y = yobs,
      x = xobs,
      yunc = yunc,
      xunc = xunc,
      link = "sqrt",
      chains = 2
    )
  )
})

testthat::test_that("test positive data example", {
  #create simulated data set
  set.seed(100)
  n <- 100
  y <- rnorm(n)
  x <- exp(rnorm(n))
  y <- exp(-0.5 + 0.2 * x + rnorm(n, sd = 0.25))
  xunc <- rep(0.1, length(x))
  yunc <- rep(0.1, length(y))
  xobs <- exp(log(x) + (rnorm(n, sd = xunc)))
  yobs <- exp(log(y) + (rnorm(n, sd = yunc)))
  f1 <-
    mpiBpred::estimateRelationship(
      y = yobs,
      x = xobs,
      yunc = yunc,
      xunc = xunc,
      link = "log",
      chains = 2
    )
  
  testthat::expect_type(f1, "list")
  testthat::expect_length(f1, 5)
  testthat::expect_equal(f1$type, 4)
  testthat::expect_equal(mean(f1$slope), 0.1175, tolerance = 0.01)
  
  f2 <-
    mpiBpred::estimateRelationship(
      y = yobs,
      x = xobs,
      yunc = yunc,
      xunc = xunc,
      link = "sqrt",
      chains = 2
    )
  
  testthat::expect_type(f2, "list")
  testthat::expect_length(f2, 5)
  testthat::expect_equal(f2$type, 3)
  testthat::expect_equal(mean(f2$slope), 1.388541e-05, tolerance = 5E-04)
})
