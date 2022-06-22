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
    downloadButton(ns("exportExecuteSummary"), "Export")
  )
}


#' @rdname shinyModule
#' @param exportData data to be exported
#' 
#' @export
exportPopUpServer <- function(id, exportData) {
  moduleServer(id,
               function(input, output, session) {
                 output$exportExecuteSummary <- downloadHandler(
                   filename = function(){
                     paste("Summary", input$exportType, sep = ".")
                   },
                   content = function(file){
                     switch(
                       input$exportType,
                       csv = exportCSV(file, exportData(), input$colseparator, input$decseparator),
                       xlsx = exportXLSX(file, exportData()),
                       json = exportJSON(file, exportData())
                     )
                   }
                 )
               })
}
