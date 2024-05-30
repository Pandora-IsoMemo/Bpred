#' Compute the distribution of the y_i, by defining a functional form
#' 
#' @param relationship A formula. The formula should contain the x-values defined in the indVars-parameter and the regression functions defined in the regfunctions parameter.
#' @param regfunctions A list. This named list should contain the regression function objects fitted by \code{\link{fitModel}} used in the relationship formula.
#' @param indVars A character vector. Contains the column names of the independent variables.
#' @param indVarsUnc A character vector. Contains the column names of the uncertainties (in sd) of the independent variables (Optional).
#' @param data A data.frame. Should contain the independent variables and optionally their uncertainties as well as a category variable.
#' @param category A character. An additional category variable in the data set (Optional).
#' @param n_samples An integer. Number of samples of Y distribution.
#' @param includeRegUnc A boolean. Should the uncertainty in the regression parameters be accounted for?
#' @param distribution An character. Distribution of the independent variables. Must be one of "normal", "lognormal" or "gamma"
#' @param rangeRestrict A boolean. Should the Y distribution be truncated to the rangeY parameter values?
#' @param rangeY An numeric Vector of two numeric values that determine optional range of values
#' @param imputeMissing impute missings in measure matrix by pmm-method of mice package or delete rows with missings?
#' 
#' @return A list of distributions (posterior samples) for each single y_i, all y_i combined and (optionally) each category
#' @examples 
#' \dontrun{
#'  #create simulated data set
#' n <- 100
#' y <- rnorm(n)
#' x <- rnorm(n)
#' y <- 1.5 + 2 * x + rnorm(n)
#' xunc <- rep(0.25, length(x))
#' yunc <- rep(0.25, length(y))
#' xobs <- x + (rnorm(n, sd = xunc))
#' yobs <- y + (rnorm(n, sd = yunc))

#' #second formula
#' y <- 2.5 - 0.2 * x + rnorm(n)
#' yobs2 <- y + (rnorm(n, sd = yunc))
#' form <- paste0("{slope} * [x] + {intercept}")
#' 
#' #estimate formulas
#' f1 <- fitModel(y = yobs,X = data.frame(x= xobs),yUnc = yunc,xUnc = data.frame(xunc= xunc),
#' form = form,chains = 2,parNames = c("slope", "intercept"), varNames = "x", shinyUse = FALSE)
#' f2 <- fitModel(y = yobs2,X = data.frame(x= xobs),yUnc = yunc,xUnc = data.frame(xunc= xunc),
#' form = form,chains = 2,parNames = c("slope", "intercept"), varNames = "x", shinyUse = FALSE)
#' 
#' data <- data.frame(Category = c("Site1", "Site1", "Site1", "Site2", "Site2"),
#' X1 = c(1, 0.9, 1.2, 4, 5),
#' SD_X1 = c(0.2, 0.3, 0.2, 0.2, 0.3),
#' X2 = c(1.5, 1.8, 1.1, 2.25, 2.3),
#' SD_X2 = c(0.5, 0.3, 0.2, 0.2, 0.3))
#'
#' yEstimates <- estimateY(relationship = "Y ~ 3 + 4.5 * f1([X1]) * f2([X2]) - f1([X1])",
#'                         regfunctions = list(f1 = f1, f2 = f2),
#'                         indVars = c("X1", "X2"),
#'                         data = data,
#'                         indVarsUnc = c("SD_X1", "SD_X2"),
#'                         category = "Category",
#'                         n_samples = 10000,
#'                         includeRegUnc = TRUE)
#' 
#' plotDensities(yEstimates, type = "Individual", plotType = "KernelDensity")
#' plotDensities(yEstimates, type = "Combined", plotType = "KernelDensity")
#' plotDensities(yEstimates, type = "Category", plotType = "KernelDensity")
#' 
#' #result statistics and comparison with reference sample
#' results <- summariseEstimates(yEstimates, type = "Individual", probability = 0.95,
#' referenceType = "dist",
# 'referenceDist = "norm", referenceParameters = c(50, 3),
#' referenceSample = c(60, 52, 75, 48, 50, 56))
#' 
#' #Alternative: Start Shiny-App
#' shiny::runApp(paste0(system.file(package = "Bpred"),"/app"))
#' }
#' @export
estimateY <- function(relationship, regfunctions,
                      indVars, indVarsUnc = "",
                      category = "", data, n_samples = 10000,
                      includeRegUnc = TRUE,
                      distribution = "normal",
                      rangeRestrict = FALSE,
                      rangeY = c(-Inf, +Inf),
                      imputeMissing = TRUE){
  if((length(indVarsUnc) != length(indVars)) | any(indVarsUnc == "") | any(is.null(indVarsUnc))){
    indVarsUnc <- paste0(indVars, "_unc")
    data[,indVarsUnc] <- 0
  }

  if(imputeMissing & any(is.na(data))){
    if(category != ""){
      if(inherits(data[, category], "character")){
        data[, category] <- factor(data[, category])
      }
    }
    imputed_Data <- mice(data, m=10, maxit = 50, seed = 500, printFlag = FALSE)
    completed <- complete(imputed_Data, "all")
    new_data <- data
    new_data[, indVarsUnc][is.na(new_data[, indVarsUnc])] <- 0
    for (i in 1:length(indVars)){
      new_data[, indVars[i]] = rowMeans(sapply(1:length(completed), function(x) completed[[x]][,indVars[i]]))
      if(any(indVarsUnc != "")){
      new_data[, indVarsUnc[i]] = new_data[, indVarsUnc[i]] + apply(sapply(1:length(completed), function(x) completed[[x]][,indVars[i]]),1,sd)
      }
    }
    if(category != ""){
      if(inherits(data[, category], "factor")){
      new_data[, category] <- apply(sapply(1:length(completed), function(x) completed[[x]][,category]), 1, getMode)
    }
    }
    data <- new_data
  } else {
    relevantVars <- indVars
    if(category != ""){
      relevantVars <- c(relevantVars, category)
    }
    if(any(indVarsUnc != "")){
      relevantVars <- c(relevantVars, indVarsUnc)
    }
    data <- na.omit(data[,c(relevantVars)])
  }

  data[,indVarsUnc] <- data[,indVarsUnc] + 1E-6
  
  
  if(is.null(rangeY[1]) | is.na(rangeY[1])) rangeY[1] <- -Inf
  if(is.null(rangeY[2]) | is.na(rangeY[2])) rangeY[2] <- Inf
        
  if (grepl("~", relationship)){
    relationship <- strsplit(relationship, "~")[[1]][2]
  }
  
  for (i in names(regfunctions)){
    relationship <- paste0(gsub(paste0(i,"\\("), paste0("f_funcs", "$", i, "(regfunctions$", i, ")(c("), relationship))
  }
  relationship <- gsub(paste0("\\]\\)"), paste0("\\]\\)\\)"), relationship)

  
  if (!includeRegUnc) {
    f_funcs <- lapply(regfunctions, function(f) {
      f_func <- function(f) {
        if(f$custom){
          form <- f$form
          return(function(x) {
            for(i in 1:NCOL(f$beta)){
              form <- gsub(paste0("\\{", f$parNames[i], "\\}"), mean(f$beta[, i]), form)
            }
            for(j in 1:length(f$varNames)){
              form <- gsub(paste0("\\[", f$varNames[j], "\\]"), paste0("x[", j, "]"), form)
            }
            eval(parse(text = form))
          })
        } else { 
        if (f$type == 1){
          return(function(x) { mean(f$intercept) + mean(f$slope) * x})
        } else if (f$type == 2) {
          return(function(x) { mean(f$slope) * x})
        } else if (f$type == 3) {
          return(function(x) { (mean(f$intercept) + mean(f$slope) * x) ^ 2})
        } else if (f$type == 4) {
          return(function(x) { exp(mean(f$intercept) + mean(f$slope) * x)})
        }
        }
      }
    })
  } else if (includeRegUnc) {
    f_funcs <- lapply(regfunctions, function(f) {
      f_func <- function(f) {
        if(f$custom){
          form <- f$form
          return(function(x) {
            s <- sample(1:nrow(f$beta), 1)
            for(i in 1:NCOL(f$beta)){
              form <- gsub(paste0("\\{", f$parNames[i], "\\}"), f$beta[s, i], form)
            }
            for(j in 1:length(f$varNames)){
              form <- gsub(paste0("\\[", f$varNames[j], "\\]"), paste0("x[", j, "]"), form)
            }
            eval(parse(text = form))
            })
        } else {
        if (f$type == 1){
          return(function(x) {
            s <- sample(1:length(f$slope), 1)
            f$intercept[s] + f$slope[s] * x})
          
        } else if (f$type == 2) {
          return(function(x) {
            s <- sample(1:length(f$slope), 1)
            f$slope[s] * x})
          
        } else if (f$type == 3) {
          return(function(x) {
            s <- sample(1:length(f$slope), 1)
            (f$intercept[s] + f$slope[s] * x) ^ 2})
          
        } else if (f$type == 4) {
          return(function(x) {
            s <- sample(1:length(f$slope), 1)
            exp(f$intercept[s] + f$slope[s] * x)})
        }
        }
      }
    }
    )
  }
  
  tempFunction <- function(relationship, values, indVars){
    for (i in 1:length(values)){
      relationship <- gsub(pattern = paste0("\\[", indVars[i], "\\]"), replacement = values[i], x = relationship)
    }
    
    ret <- try({eval(parse(text = relationship))}, silent = TRUE)
    if(inherits(ret, "try-error")){
      ret <- "Independent variables or regression function not found in your formula. Please review your formula and regression functions to ensure all independent variables are included. Also check for correct placement of brackets or if brackets are missing."
    }
      ret
  }
  
  
  Y_Samples_Individual <- lapply(1:nrow(data), function(i){
    values <- data[rep(i, n_samples), indVars]
    if ((any(indVarsUnc == "") || any(is.null(indVarsUnc))) && length(regfunctions) > 0){
      # vars <- (unlist(lapply(regfunctions, function(x) x$sdX)) ^ 2)
      # means <- (unlist(lapply(regfunctions, function(x) x$meanX)) /
      #             (unlist(lapply(regfunctions, function(x) x$sdX))) ^ 2)
      vars <- rep(0, length(indVars))
      means <- unlist(data[i, indVars])
    } 
    if(!(any(indVarsUnc == "") || any(is.null(indVarsUnc))) && length(regfunctions) > 0) {
      # vars <- (1 / (1 / unlist(data[i, indVarsUnc]) ^ 2 + 1 /
      #                 (unlist(lapply(regfunctions, function(x) x$sdX)) ^ 2)))
      # means <- unlist(vars * data[i, indVars] / data[i, indVarsUnc] ^ 2 +
      #                   (unlist(lapply(regfunctions, function(x) x$meanX)) /
      #                      (unlist(lapply(regfunctions, function(x) x$sdX))) ^ 2))
      vars <- unlist(data[i, indVarsUnc]) ^ 2
      means <- unlist(data[i, indVars])
    }
    if (!(any(indVarsUnc == "") || any(is.null(indVarsUnc))) && length(regfunctions) == 0){
      vars <- unlist(data[i, indVarsUnc]) ^ 2
      means <- unlist(data[i, indVars])
    }
    if ((any(indVarsUnc == "") || any(is.null(indVarsUnc))) && length(regfunctions) == 0){
      vars <- rep(0, length(indVars))
      means <- unlist(data[i, indVars])
    }
    if(is.null(means)){
      return("No matching independent variables found. Please check relationship and independent variables specification.")
    }
      if (distribution == "normal"){
        if(!rangeRestrict){
      values <- matrix(unlist(lapply(1:length(means), function(x)
        rnorm(n_samples, mean = means[x], sd = sqrt(vars[x])))),
        ncol = length(means))
        } else {
          values <- matrix(unlist(lapply(1:length(means), function(x)
            rnorm(n_samples, mean = means[x], sd = sqrt(vars[x])))),
            ncol = length(means))
        }
      }
      if (distribution == "lognormal"){
        if (any(means <= 0)){
          return("Non-positive values. Please use other distributions or provide suitable values")
        }
        if(!rangeRestrict){
      values <- matrix(unlist(lapply(1:length(means), function(x)
        rlnorm(n_samples, meanlog = log(means[x] / sqrt(1 + sqrt(vars[x]) / means[x] ^ 2)),
                                        sdlog = log(1 + sqrt(vars[x]) / means[x] ^ 2)))),
        ncol = length(means))
      } else {
        values <- matrix(unlist(lapply(1:length(means), function(x)
          rlnorm(n_samples, meanlog = log(means[x] / sqrt(1 + sqrt(vars[x]) / means[x] ^ 2)),
                 sdlog = log(1 + sqrt(vars[x]) / means[x] ^ 2)))),
          ncol = length(means))
      }
      }
      if (distribution == "gamma"){
        if (any(means <= 0)){
          stop("Non-positive values. Please use other distributions or provide suitable values")
        }
        if(!rangeRestrict){
        values <- matrix(unlist(lapply(1:length(means), function(x)
          rgamma(n_samples, shape = means[x] ^ 2 / vars[x],
                 rate = means[x] / vars[x]))),
          ncol = length(means))
      } else {
        values <- matrix(unlist(lapply(1:length(means), function(x)
          rgamma(2 * n_samples, shape = means[x] ^ 2 / vars[x],
                 rate = means[x] / vars[x]))),
          ncol = length(means))
      }
    
      }
    retSamples <- unlist(lapply(1:n_samples, function(x) tempFunction(relationship, values[x, ], indVars)))
    retSamples <- retSamples[retSamples >= rangeY[1] & retSamples <= rangeY[2]]
    if(length(retSamples) < 10){
      return("Restriction values do not fit to the formula. Please widen or omit them")
    }
    retSamples <- retSamples[sample(1:length(retSamples), n_samples, replace = TRUE)]
  })
  if(inherits(Y_Samples_Individual[[1]], "character")) return(Y_Samples_Individual[[1]][1])
  
  Y_Samples_Combined <- unlist(Y_Samples_Individual)
  
  if (category != ""){
    Y_Samples_Category <-
      lapply(unique(data[, category]), function(x) 
        unlist(Y_Samples_Individual[which(data[, category] == x)]))
    names(Y_Samples_Category) <- unique(data[, category])
    Y_Samples_Category_Mean <-
      lapply(unique(data[, category]), function(x) 
        Reduce("+", Y_Samples_Individual[which(data[, category] == x)]) / length(which(data[, category] == x)))
    names(Y_Samples_Category_Mean) <- unique(data[, category])
    
    return(list(Y_Samples_Individual = Y_Samples_Individual,
                Y_Samples_Combined = Y_Samples_Combined,
                Y_Samples_Category = Y_Samples_Category,
                Y_Samples_Category_Mean = Y_Samples_Category_Mean,
                relationship = relationship, regfunctions = regfunctions,
                indVars = indVars, indVarsUnc  = indVarsUnc,
                category = category, data = data, n_samples = n_samples,
                includeRegUnc = includeRegUnc,
                distribution = distribution))
    
  }
  
  return(list(Y_Samples_Individual = Y_Samples_Individual,
              Y_Samples_Combined = Y_Samples_Combined,
              relationship = relationship, regfunctions = regfunctions,
              indVars = indVars, indVarsUnc  = indVarsUnc,
              category = category, data = data, n_samples = n_samples,
              includeRegUnc = includeRegUnc,
              distribution = distribution))
}
