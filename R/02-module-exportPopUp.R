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
