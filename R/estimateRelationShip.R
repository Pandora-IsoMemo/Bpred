
linkToType <- function(link) {
  if (link == "linIntcp") {
    type <- 1
  } else if (link == "linNoIntcp") {
    type <- 2
  } else if (link == "sqrt") {
    type <- 3
  } else if (link == "log") {
    type <- 4
  } else {
    stop('link must be one of "linIntcp", "linNoIntcp", "sqrt", "log"')
  }
  type
}

linkToForm <- function(link) {
  if (link == "linIntcp") {
    type <- "{beta} * [x] + {alpha}"
  } else if (link == "linNoIntcp") {
    type <- "{beta} * [x]"
  } else if (link == "sqrt") {
    type <- "{beta} * sqrt([x]) + {alpha}"
  } else if (link == "log") {
    type <- "{beta} * log([x]) + {alpha}"
  } else {
    stop('link must be one of "linIntcp", "linNoIntcp", "sqrt", "log"')
  }
  type
}

#' Fit Model
#' 
#' @param y Measurements of the dependent variable.
#' @param X Measurements of the independent variable.
#' @param yUnc A numeric vector. y uncertainties 
#' @param xUnc A numeric vector. x uncertainties
#' @param parNames names of parameter
#' @param iter An integer. Number of iterations for MCMC model
#' @param chains An integer. Number of chains for MCMC model
#' @param burnin An integer. Number of burnin iterations for MCMC model
#' @param varNames A character. Name of x variable
#' @param form form
#' @param startPar numeric vector of length of parNames
#' @param thinning thinning
#' @param parNamesDir parNamesDir
#' @export
fitModel <- function(X, y, yUnc, xUnc, parNames, varNames, form, startPar = rep(0, length(parNames)),
                     iter = 1000, chains = 8, burnin = 0.4*iter, thinning = 5, parNamesDir = NULL){
  n <- length(y)
  formOrig <- form
  XOrig <- X
  #form <- gsub("\\[|\\]", "", form)
  burnInProp <- 0.4
  thinning <- thinning
  for(l in 1:length(varNames)){
    form <- gsub(paste0("\\[", varNames[l], "\\]"), paste0("X[, '", varNames[l], "']"), form)
  }
  
  f <- function(x){
    formOld <- form
    for(i in 1:length(parNames)){
      formOld <- gsub(paste0("\\{", parNames[i], "\\}"), x[i], formOld)
    }
    ##
    yPredOld = eval(parse(text = formOld))
    sum((y - yPredOld) ^ 2)
  }
  #test
  test <- f(startPar)
  if(is.nan(test)){
    return("Formula does not work on data. Possibly wrong transformations on negative values.")
  }

  
  startPar <- optim(startPar, f, method = c("Nelder-Mead"))$par
  
  #for restricted parameters
  if(!is.null(parNamesDir)){
    dirMatch <- match(parNamesDir, parNames)
    dirNoMatch <- which(!(dirMatch %in% 1:length(parNames)))
    startPar[dirMatch] <- 1 / length(dirMatch)
  }
  
  
  pars <- startPar
  
  usedsamples <- c(sapply(1:chains, function(i) (i-1) * iter + seq(from = burnin + 1, to = iter, by = thinning)))
  
  #####################################
  ###Starting Values
  #####################################
  #Chain 1
  sigma <- 1
  
  ######################################
  ###Tuningparameter der a-priori Verteilungen:
  ######################################
  a.eps <- 1E-5
  b.eps <- 1E-5
  a.mu <- 1E-5
  b.mu <- 1E-5
  
  #######################################
  ###Parametermatrizen zur Speicherung der MCMC Iterationen
  #######################################
  taumc <- matrix(ncol = 1, nrow = iter * chains)
  smc <- matrix(ncol = 1, nrow = iter * chains)
  betamc <- matrix(ncol = length(pars), nrow = iter * chains)
  yPredmc <- matrix(ncol = length(y), nrow = iter * chains)
  acceptMC <- matrix(ncol = length(pars), nrow = iter * chains)
  ########################################
  #MCMC-Algorithmus
  ########################################
  #changeX <- which(data$Uncertainty2 > 0)
  
  MHPar <- rep(0.1, length(pars))
  
  #rescale
  mRe <- mean(y)
  sRe <- sd(y)
  # y <- (y - mRe) / sRe
  
  # if(!is.null(data$independentUncertainty)){
  #   data$independentUncertainty <- data$independentUncertainty / sRe
  # }
  
  YMean <- y
  yPredNew <- rep(0, length(y))

  MCMC_PlotR <- function(start, iter){
    for (m in start:iter) {
      #Betas
      
      if(m %% 100 == 0){
        accRates <- colMeans(acceptMC[(m - 99):(m-1), , drop = FALSE])
        MHPar[accRates < 0.234] <<- MHPar[accRates < 0.234] * 0.9
        MHPar[accRates > 0.234] <<- MHPar[accRates > 0.234] * 1.1
      }
      #MH-step for parameters
      for(j in 1:length(pars)){
        formOld <- form
        for(i in 1:length(parNames)){
          formOld <- gsub(paste0("\\{", parNames[i], "\\}"), pars[i], formOld)
        }
        ##
        yPredOld = eval(parse(text = formOld))
        parsNew <- pars
        if(!is.null(parNamesDir) && (j %in% dirMatch)){
          parsNew[dirMatch] <- parsNew[dirMatch] * rgamma(2, shape = MHPar[dirMatch]^-2, rate = MHPar[dirMatch]^-2)
          parsNew[dirMatch] <- parsNew[dirMatch] / sum(parsNew[dirMatch])
        } else {
          parsNew[j] <- parsNew[j] + rnorm(1, sd = MHPar[j])
        }
        
        
        
        formNew <- form
        for(i in 1:length(parNames)){
          formNew <- gsub(paste0("\\{", parNames[i], "\\}"), parsNew[i], formNew)
        }
        ##
        yPredNew = eval(parse(text = formNew))
        if(all(!is.na(yPredNew))){
          acc <- pmin(1, exp(sum((y - yPredNew) ^ 2 / (-2 * sigma)) - sum((y - yPredOld) ^ 2 / (-2 * sigma))))
          
          if(!is.nan(acc)){
            randomAlpha <- runif(1)
            accept <- (acc > runif(1))
          } else {
            accept <- FALSE
          }
        } else {
          accept <- FALSE
        }
        if(accept){
          if(!is.null(parNamesDir) && (j %in% dirMatch)){
            pars[dirMatch] <- parsNew[dirMatch]
          } else {
            pars[j] <- parsNew[j]
          }
        }
        acceptMC[m, j] <<- accept
      }
      pars <<- pars
      # #MH-step for time
      if(m %% 10 == 0 && (NCOL(xUnc) == NCOL(X))){
      for(l in 1:NCOL(xUnc)){
        changeX <- which(xUnc[, l] > 0)
      
      if (length(changeX) > 0){
        formNew <- form
        random <- rnorm(n, sd = xUnc[, l])
        XNew <- X
        XNew[, l] <- X[, l] + random
        muX <- mean(X[, l])
        sdX <- sd(X[,l])
        for(i in 1:length(parNames)){
          formNew <- gsub(paste0("\\{", parNames[i], "\\}"), pars[i], formNew)
        }
        formOld <- formNew
        formNew <- gsub(paste0("X\\["), "XNew\\[", formNew)
        ##
        yPredNew = eval(parse(text = formNew))
        yPredOld = eval(parse(text = formOld))
        
        acc <- pmin(1, exp(((y - yPredNew) ^ 2 / (-2 * sigma)) + 
                             (XNew[, l] - XOrig[, l])^2 / (-2 * xUnc[, l]^2) +
                             (XNew[, l] - muX)^2 / (-2 * sdX^2) - 
                             ((y - yPredOld) ^ 2 / (-2 * sigma)) - 
                             (X[, l] - XOrig[, l])^2 / (-2 * xUnc[, l]^2) -
                             (X[, l] - muX)^2 / (-2 * sdX^2)))
        acc[is.na(acc)] <- 0
        randomAlpha <- runif(n)
        updated <- which(randomAlpha < acc)
        if(length(updated) > 0){
        X[updated, l] <- XNew[updated, l]
        }
      }
      }
      }
      
      #Sigma
      #Smoothing Parameter lambda
      
      # nolint start
      #conditional posterioris:
      formOld <- form
      for(i in 1:length(parNames)){
        formOld <- gsub(paste0("\\{", parNames[i], "\\}"), pars[i], formOld)
      }
      ##
      yPredOld = eval(parse(text = formOld))
      
      scale <- (b.eps + 0.5 * sum((((y - yPredOld)) ^ 2))) ^ - 1
      sigma <<- 1 / rgamma(1, shape = a.eps + n / 2, scale = scale)
      
      if(!is.null(yUnc)){
        sdmY <- 1 / (1 / sigma + 1 / (yUnc ^ 2 + 1E-6))
        mY <- ((yPredOld) / sigma +
                 YMean / (yUnc ^ 2 + 1E-6)) * sdmY
        y <- rnorm(length(yUnc), mY, sd = sqrt(sdmY))
      }
      
      # nolint end
      
      #Werte in Parametermatrizen einsetzen
      betamc[m, ] <<- pars
      yPredmc[m, ] <<- yPredNew
      smc[m, ]  <<-  mean(sigma)
    }
    return(betamc)
  }
  for ( k in 1:chains) {
    sigma <- 1
    MHPar <- rep(0.1, length(pars))
    showMessage(
      MCMC_PlotR,
      msg = paste0("Calculating Custom Formula Model, Chain: ",k),
      value = k / chains)(
        start = (k-1) * iter + 1, iter = k * iter
      )
  }
  
  #burnin <- round(burnInProp * iter / chains +  seq(0, iter, iter / chains))

  #Vektor der tatsaechlich benutzten Beobachtungen

  return(list(beta = betamc[usedsamples, ], sigma = smc[usedsamples, ],
              mRe = mRe, sRe = sRe, yPredmc = yPredmc[usedsamples, ],
              acceptMC = acceptMC, form = formOrig,
              parNames = parNames, varNames = varNames, nChains = chains,
              custom = TRUE))
}

showMessage <- function(fun, msg = "Loading ...", ...) {
  force(fun)
  args <- c(list(...), message = msg)
  function(...) {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    do.call(progress$set, args)
    fun(...)
  }
}
