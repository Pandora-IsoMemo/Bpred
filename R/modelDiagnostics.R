#' Convergence Diagnostics
#' 
#' @param parameters parameters
#' @param nChains number of chains
#'
#' @export
convergenceDiagnostics <- function(parameters, nChains){
  splitChains <- factor(rep(1:nChains, each = nrow(parameters) / nChains))
  
  mcmcObject <- split(parameters, splitChains)
  
  mcmcObject <- lapply(mcmcObject, function(x){
    x <- as.matrix(x)
    x <- mcmc(x, start = 1, end = nrow(x))
    x
  })
  
  raftery <- try({raftery.diag(parameters)}, silent = TRUE)
  gelman <- try({gelman.diag(mcmcObject, autoburnin = FALSE, multivariate = FALSE)}, silent = TRUE)
  geweke <- try({geweke.diag(mcmcObject)}, silent = TRUE)
  heidel <- try({heidel.diag(parameters)}, silent = TRUE)
  
  if(nChains == 1){
    gelman <- "For Gelman-Rubin diagnostics, at least 2 chains are required.
    Number of chains option available in the model options tab"
  }
  
  return(list(raftery = raftery, gelman = gelman, geweke = geweke, heidel = heidel))
}

#' Text Export Button UI
#' 
#' @export
#' @rdname shinyModule
textExportButton <- function(id, title = "Download") {
  ns <- NS(id)
  downloadButton(ns("download"), title)
}

#' Text Export Server
#' 
#' @export
#' @rdname shinyModule
#' @param printFun print function
#' @param filename (character) file name
textExport <- function(input, output, session, printFun, filename = "output") {
  content <- reactive({
    capture.output(printFun()())
  })
  
  output$text <- renderPrint({
    printFun()()
  })
  
  output$download <- downloadHandler(
    filename = function(){
      paste0(filename, ".txt")
    },
    content = function(file) {
      writeLines(content(), file)
    }
  )
}
