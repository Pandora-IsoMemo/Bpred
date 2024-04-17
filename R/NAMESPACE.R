#' The 'mpiBpred' package.
#'
#' @description A DESCRIPTION OF THE PACKAGE
#'
#' @docType package
#' @name mpiBpred
#' @aliases mpiBpred
#' @import shiny
#' @importFrom coda raftery.diag gelman.diag geweke.diag heidel.diag mcmc
#' @importFrom dplyr bind_cols bind_rows filter group_by if_else mutate summarise ungroup
#' @importFrom ggplot2 aes aes_ aes_string geom_boxplot geom_density geom_errorbar geom_histogram 
#' ggplot geom_point ggtitle geom_line geom_ribbon theme
#' @importFrom graphics lines
#' @importFrom httr content
#' @importFrom DataTools checkAnyNonNumericColumns downloadModelUI downloadModelServer importDataUI 
#'  importDataServer tryCatchWithWarningsAndErrors
#' @importFrom magrittr %>%
#' @importFrom grDevices dev.off pdf png svg tiff
#' @importFrom mice mice complete
#' @importFrom modules module
#' @importFrom rlang .data
#' @importFrom rsync rsync getData sendObject listFiles
#' @importFrom shinyjs alert reset runjs useShinyjs
#' @importFrom shinyMatrix matrixInput updateMatrixInput
#' @importFrom shinyTools dataExportButton dataExportServer formatTitlesOfGGplot 
#'  formatRangesOfGGplot headerButtonsUI plotExportButton plotExportServer 
#'  plotRangesUI plotRangesServer plotTitlesUI plotTitlesServer
#' @importFrom stats density median na.omit quantile rgamma rlnorm rnorm sd optim runif var
#' @importFrom utils capture.output combn packageVersion
#' @importFrom shinyjs alert
#' @importFrom yaml read_yaml
#'
globalVariables(c("model", "dataObj", "formulasObj", "inputObj"))
NULL

#' Server and UI Functions for Shiny Module
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session
#' @param id namespace id
#' @param title title of tab in tabset panel
#'
#' @name shinyModule
NULL
