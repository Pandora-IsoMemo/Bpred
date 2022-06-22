#' @rdname shinyModule
#' 
#' @export
exportPopUpUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    selectInput(
      ns("exportType"),
      "File type",
      choices = c("csv", "xlsx", "json"),
      selected = "xlsx"
    ),
    conditionalPanel(
      condition = "input['exportType'] == 'csv'",
      div(style = "display: inline-block;horizontal-align:top; width: 80px;",
          textInput(ns("colseparator"), "column separator:", value = ",")),
      div(style = "display: inline-block;horizontal-align:top; width: 80px;",
          textInput(ns("decseparator"), "decimal separator:", value = "."))
    ),
    downloadButton(ns("exportExecute"), "Export")
  )
}


#' @rdname shinyModule
#' @param exportData data to be exported
#' @param filename (character) name of file (without extension)
#' 
#' @export
exportPopUpServer <- function(id, exportData, filename) {
  moduleServer(id,
               function(input, output, session) {
                 output$exportExecute <- downloadHandler(
                   filename = function(){
                     paste(filename, input$exportType, sep = ".")
                   },
                   content = function(file){
                     withProgress({
                     switch(
                       input$exportType,
                       csv = exportCSV(file, exportData(), input$colseparator, input$decseparator),
                       xlsx = exportXLSX(file, exportData()),
                       json = exportJSON(file, exportData())
                     )},
                     value = 0.75,
                     message = 'Exporting data ...')
                   }
                 )
               })
}


#' @rdname shinyModule
#' 
#' @export
exportPlotPopUpUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    plotOutput(ns("preview")),
    selectInput(
      ns("exportType"),
      "Filetype",
      choices = c("png", "pdf", "svg", "tiff")
    ),
    numericInput(ns("width"), "Width (px)", value = 1280),
    numericInput(ns("height"), "Height (px)", value = 800),
    downloadButton(ns("exportExecute"), "Export"),
  )
}


#' @rdname shinyModule
#' @param exportPlot plot to be exported
#' @param filename (character) name of file (without extension)
#' 
#' @export
exportPlotPopUpServer <- function(id, exportPlot, filename) {
  moduleServer(id,
               function(input, output, session) {
                 output$preview <- renderPlot({ 
                   exportPlot()
                   })
                 
                 output$exportExecute <- downloadHandler(
                   filename = function(){
                     paste(filename, input$exportType, sep = ".")
                   },
                   content = function(file){
                     withProgress({
                       switch(
                         input$exportType,
                         png = png(file, width = input$width, height = input$height),
                         pdf = pdf(file, width = input$width / 72, height = input$height / 72),
                         tiff = tiff(file, width = input$width, height = input$height),
                         svg = svg(file, width = input$width / 72, height = input$height / 72)
                       )
                       print(exportPlot())
                       dev.off()
                       },
                       value = 0.75,
                       message = 'Exporting plot ...')
                   }
                 )
               })
}
