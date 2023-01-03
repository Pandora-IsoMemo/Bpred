#' Download model module
#'
#' UI function to download a zip file with notes and a list of models
#'
#' @param id id of module
#' @param label label of module
#'
#' @rdname downloadModel
#'
#' @export
downloadModelUI <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    tags$h5(label),
    textAreaInput(ns("notes"), "Notes"),
    HTML("<br>"),
    downloadButton(ns("downloadModel"), "Download"),
    tags$br()
  )
}


#' Server function download model
#'
#' Backend for download model module
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param allParentInput (reactive) list of inputs from parent module
#' @param yEstimates (reactive) An object created by \code{\link{estimateY}}.
#'  Distributions of the dependent variable.
#' @param formulas (reactive) formulas
#' @param data (reactive) data
#' @param uploadedNotes (reactive) variable that stores content of README.txt
#'
#' @export
downloadModel <-
  function(input,
           output,
           session,
           allParentInput,
           yEstimates,
           formulas,
           data,
           uploadedNotes) {
    observe({
      updateTextAreaInput(session, "notes", value = uploadedNotes())
    })
    
    output$downloadModel <- downloadHandler(
      filename = function() {
        paste(gsub("\ ", "_", Sys.time()), "bpred.zip", sep = "_")
      },
      content = function(file) {
        zipdir <- tempdir()
        modelfile <- file.path(zipdir, "model.Rdata")
        notesfile <- file.path(zipdir, "README.txt")
        helpfile <- file.path(zipdir, "help.html")
        
        model <- yEstimates()
        formulasObj <- reactiveValuesToList(formulas)
        dataObj <- reactiveValuesToList(data)
        inputObj <- allParentInput()
        save(model, formulasObj, dataObj, inputObj, file = modelfile)
        writeLines(input$notes, notesfile)
        save_html(getHelp(input$tab), helpfile)
        zip::zipr(file, c(modelfile, notesfile, helpfile))
      }
    )
  }


#' Upload model module
#'
#' UI function to upload a zip file with notes and a list of models
#'
#' @param id id of module
#' @param label label of module
#'
#' @rdname uploadModel
#'
#' @export
uploadModelUI <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    HTML("<br>"),
    tags$h5(label),
    fileInput(ns("uploadModel"), label = "Upload local model"),
    selectInput(
      ns("remoteModel"),
      label = "Select remote model",
      choices = dir(file.path(settings$pathToSavedModels)) %>%
        sub(pattern = '\\.zip$', replacement = ''),
      selected = NULL
    ),
    actionButton(ns("loadRemoteModel"), "Load Remote Model")#,
    #helpText("Remote models are only available on on https://isomemoapp.com")
  )
}


#' Server function upload model
#'
#' Backend for upload model module
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#'
#' @export
uploadModel <-
  function(input,
           output,
           session) {
    pathToModel <- reactiveVal(NULL)
    
    uploadedData <- reactiveValues(
      inputFields = NULL,
      data = NULL,
      formulas = NULL,
      model = NULL,
      notes = NULL
    )
    
    observeEvent(input$uploadModel, {
      pathToModel(input$uploadModel$datapath)
    })
    
    observeEvent(input$loadRemoteModel, {
      
      # USE THIS AS FALL BACK IF NO INTERNET CONNECTION
      # pathToModel(file.path(
      #   settings$pathToSavedModels,
      #   paste0(input$remoteModel, ".zip")
      # ))
      
      # add new function getRemoteModelsFromGH() to fill choices of remote models
      
      tmp <- tempfile()
      res <- try(download.file(
        "https://github.com/Pandora-IsoMemo/bpred/raw/main/inst/app/predefinedModels/2020-04-15_18_59_33_bpred.zip",
        destfile = tmp))

      pathToModel(tmp)
    })
    
    observeEvent(pathToModel(), {
      res <- try({
        zip::unzip(pathToModel())
        load("model.Rdata") # should contain: model, formulasObj, dataObj, inputObj
        uploadedData$notes <- readLines("README.txt")
      })
      
      if (inherits(res, "try-error")) {
        shinyjs::alert("Could not load file.")
        return()
      }
      
      if (!exists("model") ||
          !exists("formulasObj") ||
          !exists("dataObj") || !exists("inputObj")) {
        shinyjs::alert("File format not valid. Model object not found.")
        return()
      }
      
      uploadedData$data <- dataObj
      uploadedData$formulas <- formulasObj
      uploadedData$model <- model
      uploadedData$inputFields <- inputObj
      
      alert("Model loaded")
    })
    
    return(uploadedData)
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
