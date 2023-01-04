#' @rdname shinyModule
#' 
#' @export
remoteModelsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    actionButton(ns("loadRemoteModel"), "Load Remote Model")
    #helpText("Remote models are only available on on https://isomemoapp.com")
  )
  
}

#' @rdname shinyModule
#' 
#' @export
remoteModelsServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 pathToRemote <- reactiveVal(NULL)
                 
                 observeEvent(input$loadRemoteModel, {
                   
                   # USE THIS AS FALL BACK IF NO INTERNET CONNECTION
                   # pathToRemote(file.path(
                   #   settings$pathToSavedModels,
                   #   paste0(input$remoteModel, ".zip")
                   # ))
                   
                   # add new function getRemoteModelsFromGH() to fill choices of remote models
                   
                   tmp <- tempfile()
                   res <- try(download.file(
                     "https://github.com/Pandora-IsoMemo/bpred/raw/main/inst/app/predefinedModels/2020-04-15_18_59_33_bpred.zip",
                     destfile = tmp))
                   
                   pathToRemote(tmp)
                 })
                 
                 return(pathToRemote)
               })
}




#' Get Remote Models From GH
#' 
#' Get remote models from github directory
getRemoteModelsFromGH <- function() {
  # api.github.com/repos/Pandora-IsoMemo/bpred/contents/inst/app/predefinedModels
  repo <- "bpred"
  tmp <- httr::GET(paste0(
    "api.github.com/repos/Pandora-IsoMemo/", repo, "/contents/inst/app/predefinedModels"
  ))
  apiOut <- httr::content(tmp)
  
  modelNames <- lapply(apiOut, function(el) el$name) %>%
    unlist()
  
  return(modelNames)
}
