# Plot the densities estimated by [`estimateY`](https://pandora-isomemo.github.io/bpred/reference/estimateY.md).

Plot the densities estimated by
[`estimateY`](https://pandora-isomemo.github.io/bpred/reference/estimateY.md).

## Usage

``` r
plotDensities(
  yEstimates,
  type = "Sample",
  plotType = "KernelDensity",
  nBins = 15,
  meanType = "1",
  showLegend = TRUE,
  whiskerMultiplier = 0.95,
  boxQuantile = 0.68
)
```

## Arguments

- yEstimates:

  An object created by
  [`estimateY`](https://pandora-isomemo.github.io/bpred/reference/estimateY.md).
  Distributions of the dependent variable.

- type:

  A character. Show distribution by "Sample" (default), "Category" (only
  if supplied by yEstimates) or all individuals "Combined"?

- plotType:

  A character. Form of presentation, choices are "KernelDensity"
  (default), "Histogram" or "Boxplot"

- nBins:

  An integer Number of bins for histogram

- meanType:

  A character "1" for total variance for category type, "2" for mean
  variance

- showLegend:

  show legend?

- whiskerMultiplier:

  multiplier of boxplot whiskers

- boxQuantile:

  quantile of boxplot

## Value

A ggplot2 object with densities
