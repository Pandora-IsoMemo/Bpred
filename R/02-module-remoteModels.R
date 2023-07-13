#' @rdname shinyModule
#' 
#' @export
remoteModelsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    selectInput(
      ns("remoteModelChoice"),
      label = "Select remote model",
      choices = NULL,
      selected = NULL
    ),
    actionButton(ns("loadRemoteModel"), "Load Remote Model")
    #helpText("Remote models are only available on on https://isomemoapp.com")
  )
  
}

#' @rdname shinyModule
#' @param githubRepo (character) name of used github repository
#' @export
remoteModelsServer <- function(id, githubRepo = "bpred") {
  moduleServer(id,
               function(input, output, session) {
                 useLocalModels <- reactiveVal(TRUE)
                 localChoices <- reactiveVal({
                   dir(file.path(settings$pathToSavedModels)) %>%
                     sub(pattern = '\\.zip$', replacement = '')
                 })
                 remoteChoices <- reactiveVal(getRemoteModelsFromGithub(githubRepo = githubRepo))
                 
                 pathToRemote <- reactiveVal(NULL)
                 
                 observeEvent(localChoices(), {
                   updateSelectInput(session = session, "remoteModelChoice", 
                                     choices = localChoices())
                 })
                 
                 observeEvent(remoteChoices(), {
                   if (!is.null(remoteChoices())) {
                     useLocalModels(FALSE)
                     choices <- remoteChoices()
                   } else {
                     useLocalModels(TRUE)
                     choices <- localChoices()
                   }
                   
                   updateSelectInput(session = session, "remoteModelChoice", 
                                     choices = choices)
                 })
                 
                 observeEvent(input$loadRemoteModel, {
                   if (!useLocalModels()) {
                     tmpPath <- tempfile()
                     withProgress(message = "Downloading remote model ...", value = 0.9, {
                       res <- try(download.file(
                         paste0(
                           "https://github.com/Pandora-IsoMemo/", 
                           githubRepo, 
                           "/raw/main/inst/app/predefinedModels/", 
                           input$remoteModelChoice,
                           ".zip"
                         ),
                         destfile = tmpPath))
                     })
                   } else {
                     # FALL BACK IF NO INTERNET CONNECTION
                     tmpPath <- file.path(
                       settings$pathToSavedModels,
                       paste0(input$remoteModelChoice, ".zip")
                     )
                   }
                   
                   pathToRemote(tmpPath)
                 })
                 
                 return(pathToRemote)
               })
}

#' Get Remote Models From Github
#' 
#' Get remote models from github directory
#' 
#' @param apiOut (list) output of api call from getGithubContent
#' @inheritParams remoteModelsServer
getRemoteModelsFromGithub <- function(githubRepo, apiOut = NULL) {
  res <- try({
    if (is.null(apiOut)) apiOut <- getGithubContent(githubRepo = githubRepo)
    lapply(apiOut, function(el) el$name) %>%
      unlist() %>%
      sub(pattern = '\\.zip$', replacement = '')
  })
  
  if (inherits(res, "try-error")) {
    
    thisPackage <- packageName(environment(getRemoteModelsFromGithub))
    shinyjs::alert(paste(
      "No connection to the remote github folder. The 'remote models'",
      "are taken from the models that were locally saved with version",
      packageVersion(thisPackage), "of", thisPackage
    ))
    NULL
  } else {
    res
  }
}

#' Get Github Content
#' 
#' Get content of api call to github folder
#' @inheritParams remoteModelsServer
getGithubContent <- function(githubRepo) {
  # api.github.com/repos/Pandora-IsoMemo/bpred/contents/inst/app/predefinedModels
  res <- httr::GET(paste0(
    "api.github.com/repos/Pandora-IsoMemo/", githubRepo, "/contents/inst/app/predefinedModels"
  ))
  httr::content(res)
}
