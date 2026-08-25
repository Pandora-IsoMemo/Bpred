# Plot functions

Plot functions

## Usage

``` r
plotFunctions(
  data,
  xVar,
  yVar,
  object,
  PointSize = 1,
  LineWidth = 1,
  prop = 0.8,
  ...
)
```

## Arguments

- data:

  data

- xVar:

  independent variable

- yVar:

  dependent variable

- object:

  object

- PointSize:

  point size

- LineWidth:

  line width

- prop:

  (numeric) double between 0 and 1: length of credibility interval. The
  default value is 80 percent.

- ...:

  Other arguments passed on to `layer()` for display of the credibility
  interval. These are often aesthetics, used to set an aesthetic to a
  fixed value, like `colour = "red"` or `alpha = 0.5`.
