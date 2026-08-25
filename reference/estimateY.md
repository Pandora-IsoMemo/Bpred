# Compute the distribution of the y_i, by defining a functional form

Compute the distribution of the y_i, by defining a functional form

## Usage

``` r
estimateY(
  relationship,
  regfunctions,
  indVars,
  indVarsUnc = "",
  category = "",
  data,
  n_samples = 10000,
  includeRegUnc = TRUE,
  distribution = "normal",
  rangeRestrict = FALSE,
  rangeY = c(-Inf, +Inf),
  imputeMissing = TRUE
)
```

## Arguments

- relationship:

  A formula. The formula should contain the x-values defined in the
  indVars-parameter and the regression functions defined in the
  regfunctions parameter.

- regfunctions:

  A list. This named list should contain the regression function objects
  fitted by
  [`fitModel`](https://pandora-isomemo.github.io/bpred/reference/fitModel.md)
  used in the relationship formula.

- indVars:

  A character vector. Contains the column names of the independent
  variables.

- indVarsUnc:

  A character vector. Contains the column names of the uncertainties (in
  sd) of the independent variables (Optional).

- category:

  A character. An additional category variable in the data set
  (Optional).

- data:

  A data.frame. Should contain the independent variables and optionally
  their uncertainties as well as a category variable.

- n_samples:

  An integer. Number of samples of Y distribution.

- includeRegUnc:

  A boolean. Should the uncertainty in the regression parameters be
  accounted for?

- distribution:

  An character. Distribution of the independent variables. Must be one
  of "normal", "lognormal" or "gamma"

- rangeRestrict:

  A boolean. Should the Y distribution be truncated to the rangeY
  parameter values?

- rangeY:

  An numeric Vector of two numeric values that determine optional range
  of values

- imputeMissing:

  impute missings in measure matrix by pmm-method of mice package or
  delete rows with missings?

## Value

A list of distributions (posterior samples) for each single y_i, all y_i
combined and (optionally) each category

## Examples

``` r
if (FALSE) { # \dontrun{
 #create simulated data set
n <- 100
y <- rnorm(n)
x <- rnorm(n)
y <- 1.5 + 2 * x + rnorm(n)
xunc <- rep(0.25, length(x))
yunc <- rep(0.25, length(y))
xobs <- x + (rnorm(n, sd = xunc))
yobs <- y + (rnorm(n, sd = yunc))
#second formula
y <- 2.5 - 0.2 * x + rnorm(n)
yobs2 <- y + (rnorm(n, sd = yunc))
form <- paste0("{slope} * [x] + {intercept}")

#estimate formulas
f1 <- fitModel(y = yobs,X = data.frame(x= xobs),yUnc = yunc,xUnc = data.frame(xunc= xunc),
form = form,chains = 2,parNames = c("slope", "intercept"), varNames = "x", shinyUse = FALSE)
f2 <- fitModel(y = yobs2,X = data.frame(x= xobs),yUnc = yunc,xUnc = data.frame(xunc= xunc),
form = form,chains = 2,parNames = c("slope", "intercept"), varNames = "x", shinyUse = FALSE)

data <- data.frame(Category = c("Site1", "Site1", "Site1", "Site2", "Site2"),
X1 = c(1, 0.9, 1.2, 4, 5),
SD_X1 = c(0.2, 0.3, 0.2, 0.2, 0.3),
X2 = c(1.5, 1.8, 1.1, 2.25, 2.3),
SD_X2 = c(0.5, 0.3, 0.2, 0.2, 0.3))

yEstimates <- estimateY(relationship = "Y ~ 3 + 4.5 * f1([X1]) * f2([X2]) - f1([X1])",
                        regfunctions = list(f1 = f1, f2 = f2),
                        indVars = c("X1", "X2"),
                        data = data,
                        indVarsUnc = c("SD_X1", "SD_X2"),
                        category = "Category",
                        n_samples = 10000,
                        includeRegUnc = TRUE)

plotDensities(yEstimates, type = "Individual", plotType = "KernelDensity")
plotDensities(yEstimates, type = "Combined", plotType = "KernelDensity")
plotDensities(yEstimates, type = "Category", plotType = "KernelDensity")

#result statistics and comparison with reference sample
results <- summariseEstimates(yEstimates, type = "Individual", probability = 0.95,
referenceType = "dist",
referenceSample = c(60, 52, 75, 48, 50, 56))

#Alternative: Start Shiny-App
shiny::runApp(paste0(system.file(package = "Bpred"),"/app"))
} # }
```
