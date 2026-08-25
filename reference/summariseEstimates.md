# Summarise estimates and check for pairwise differences

Summarise estimates and check for pairwise differences

## Usage

``` r
summariseEstimates(
  yEstimates,
  type = "Sample",
  checkDifferences = TRUE,
  probability = 0.95,
  checkDifferencesReference = FALSE,
  referenceType = "dist",
  referenceDist = "norm",
  referenceParameters = c(0, 1),
  referenceSample = c(),
  referenceTable = matrix(data = NA, nrow = 2),
  meanType = "1"
)
```

## Arguments

- yEstimates:

  An object created by
  [`estimateY`](https://pandora-isomemo.github.io/bpred/reference/estimateY.md).
  Distributions of the dependent variable.

- type:

  A character. Summarise and compare distribution by "Individual"
  (default), "Category" or all individuals "Combined"?

- checkDifferences:

  A boolean. Check pairwise differences TRUE / FALSE. Only relevant if
  type = "Individual" or "Category"

- probability:

  A numeric. Level of the credible intervals.

- checkDifferencesReference:

  A boolean. Check differences with reference distribution or sample.

- referenceType:

  A character. Type of reference, one of: "dist", "sample" and
  "freqTable"

- referenceDist:

  A character. Type of distribution in R, defaults to "norm" (if
  referenceType = "dist")

- referenceParameters:

  A numeric vector. Parameters of reference distribution (same order as
  in corresponding r-function)

- referenceSample:

  A numeric vector. Sample from reference distribution (if referenceType
  = "sample")

- referenceTable:

  A numeric vector. Sample from reference distribution (if referenceType
  = "sample")

- meanType:

  A character "1" for total variance for category type, "2" for mean
  variance

## Value

A ggplot2 object with densities
