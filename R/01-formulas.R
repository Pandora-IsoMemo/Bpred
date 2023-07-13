#' Enrich Form
#' 
#' @param formDF (data.frame) a dataframe containing the form, name, x and y variables
#' @param parNames (character) names of parameters from form
#' @param formulasObject fitted formulas object
#' @param y y variable
#' 
#' @export
enrichForm <- function(formDF, parNames, formulasObject, y) {
  measures <- data.frame(parameter = paste(parNames,
                                           signif(colMeans(formulasObject$beta), 4), sep = " = ", collapse = ", "),
                         credible_intervals_95p = paste0(lapply(1:length(parNames),
                                                                function(x) paste(parNames[x],
                                                                                  paste(signif(quantile(formulasObject$beta[,x],c(0.025, 0.975)), 4), collapse = ","),
                                                                                  sep = " = (", collapse = ",)")), collapse = ", ", ")"),
                         R_squared = signif(1 - sum((colMeans(formulasObject$yPredmc) - y)^2) / sum((y-mean(y))^2), 4),
                         p_direction = paste0(lapply(1:length(parNames),
                                                     function(x) paste(parNames[x],
                                                                       paste(signif(max(sum(formulasObject$beta[,x] > 0),
                                                                                        sum(formulasObject$beta[,x] < 0)) / length(formulasObject$beta[,x]), 4), collapse = ","),
                                                                       sep = " = (", collapse = "")), collapse = ", ", ")"))
  
  cbind(formDF, measures)
}
