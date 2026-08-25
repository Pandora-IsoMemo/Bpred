# Fit Model

Fit Model

## Usage

``` r
fitModel(
  X,
  y,
  yUnc,
  xUnc,
  parNames,
  varNames,
  form,
  startPar = rep(0, length(parNames)),
  iter = 1000,
  chains = 8,
  burnin = 0.4 * iter,
  thinning = 5,
  parNamesDir = NULL,
  shinyUse = TRUE
)
```

## Arguments

- X:

  Measurements of the independent variable.

- y:

  Measurements of the dependent variable.

- yUnc:

  A numeric vector. y uncertainties

- xUnc:

  A numeric vector. x uncertainties

- parNames:

  names of parameter

- varNames:

  A character. Name of x variable

- form:

  form

- startPar:

  numeric vector of length of parNames

- iter:

  An integer. Number of iterations for MCMC model

- chains:

  An integer. Number of chains for MCMC model

- burnin:

  An integer. Number of burnin iterations for MCMC model

- thinning:

  thinning

- parNamesDir:

  parNamesDir

- shinyUse:

  set to TRUE for shiny use and FALSE for non-shiny use
