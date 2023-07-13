#' Enrich Form
#'
#' @param formDF (data.frame) a dataframe containing the form, name, x and y variables
#' @param parNames (character) names of parameters from form
#' @param formulasObject (list) fitted formulas object
#' @param y (numeric) y variable
#'
#' @export
enrichForm <- function(formDF, parNames, formulasObject, y) {
  measures <- data.frame(
    parameter = paste(
      parNames,
      signif(colMeans(formulasObject$beta), 4),
      sep = " = ",
      collapse = ", "
    ),
    credible_intervals_95p = paste0(lapply(1:length(parNames),
                                           function(x)
                                             paste(
                                               parNames[x],
                                               paste(signif(quantile(
                                                 formulasObject$beta[, x], c(0.025, 0.975)
                                               ), 4), collapse = ","),
                                               sep = " = (",
                                               collapse = ",)"
                                             )), collapse = ", ", ")"),
    R_squared = signif(1 - sum((
      colMeans(formulasObject$yPredmc) - y
    ) ^ 2) / sum((y - mean(
      y
    )) ^ 2), 4),
    Bayes_R_squared = signif(bayes_R2_res(
      y = y, ypred = formulasObject$yPredmc
    ), 4),
    p_direction = paste0(lapply(1:length(parNames),
                                function(x)
                                  paste(
                                    parNames[x],
                                    paste(signif(
                                      max(sum(formulasObject$beta[, x] > 0),
                                          sum(formulasObject$beta[, x] < 0)) / length(formulasObject$beta[, x]), 4
                                    ), collapse = ","),
                                    sep = " = (",
                                    collapse = ""
                                  )), collapse = ", ", ")")
  )
  
  cbind(formDF, measures)
}

#' Bayes R2 Res
#' 
#' @param y (numeric) y variable
#' @param ypred (numeric) predicted y from fitted formulas object
#' 
#' @export 
bayes_R2_res <- function(y, ypred) {
  e <- -1 * sweep(ypred, 2, y)
  var_ypred <- apply(ypred, 1, var)
  var_e <- apply(e, 1, var)
  mean(var_ypred / (var_ypred + var_e))
}
