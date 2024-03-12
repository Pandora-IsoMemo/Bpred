library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyMatrix)
library(dplyr)
library(shinycssloaders)
library(ggplot2)
library(shinyjs)
library(mpiBpred)
library(coda)

tagList(
  shinyjs::useShinyjs(),
  shiny::navbarPage(
    header = includeCSS("www/custom.css"),
    title = paste("Bpred App", packageVersion("mpiBpred")),
    theme = shinythemes::shinytheme("flatly"),
    position = "fixed-top",
    collapsible = TRUE,
    id = "tab",
    # DATA ---------------------------------------------------------------------------------------
    tabPanel(
      title = "Data/Model Import/Export",
      id = "Data",
      value = "Data",
      sidebarLayout(
        sidebarPanel(
          ## left sidebar ----
          width = 2,
          style = "position:fixed; width:15%; max-width:350px; overflow-y:auto; height:85%",
          DataTools::importDataUI("DataFile", "Import Data"),
          tags$br(), tags$br(),
          actionButton("simulateData", "Simulate Example Data"),
          tags$br(), tags$br(),
          numericInput(
            "n",
            label = "No. of Observations:",
            value = 100,
            min = 1,
            width = "75%"
          ),
          tags$hr(),
          DataTools::importDataUI("modelUpload", label = "Import Model"),
          checkboxInput("useDownload", label = "Download model"),
          conditionalPanel(
            condition = "input.useDownload == true",
            DataTools::downloadModelUI("modelDownload", label = "Download")
          )
        ),
        mainPanel(
          ## main panel ----
          dataTableOutput('data')
          ),
      )
    ),
    # FORMULAS -----------------------------------------------------------------------------------
    tabPanel(
      title = "Formulas",
      id = "Formulas",
      value = "Formulas",
      sidebarLayout(
        sidebarPanel(
          ## left sidebar ----
          width = 2,
          style = "position:fixed; width:15%; max-width:350px; overflow-y:auto; height:85%",
          h3("Define Formulas"),
          textInput("formName", "Name:", "formula_1"),
          pickerInput(
            inputId = "f_y",
            label = "y variable:",
            choices = character(0),
            options = list(
              "actions-box" = FALSE,
              "none-selected-text" = 'No variables selected',
              "max-options" = 1
            ),
            multiple = FALSE
          ),
          selectInput(
            inputId = "f_yunc",
            label = "y-uncertainty (sd) - optional:",
            choices = character(0),
            multiple = FALSE
          ),
          radioButtons(
            inputId = "selectFType",
            label = "Select formula type:",
            choices = c(
              "Univariate linear regression" = "linear",
              "Custom formula" = "custom"
            )
          ),
          conditionalPanel(
            condition = "input.selectFType == 'linear'",
            pickerInput(
              inputId = "f_x",
              label = "x variable:",
              choices = character(0),
              options = list(
                "actions-box" = FALSE,
                "none-selected-text" = 'No variables selected',
                "max-options" = 1
              ),
              multiple = FALSE
            ),
            selectInput(
              inputId = "f_xunc",
              label = "x-uncertainty (sd) - optional:",
              choices = character(0),
              multiple = FALSE
            ),
            selectInput(
              "f_link",
              "relationship (link)",
              choices = list(
                'linear' =  "linIntcp",
                'linear without Intercept' =  "linNoIntcp",
                'log-linear' = "log",
                'sqrt-linear' = "sqrt"
              )
            )
          ),
          conditionalPanel(
            condition = "input.selectFType == 'custom'",
            textInput(
              "formCustom",
              "Custom formula (variables in square brackets and parameters in curly braces):",
              "{a} * [x] + {b}"
            ),
            selectizeInput(
              "custom_x",
              label = "x variables (same names as in formula)",
              choices = character(0),
              multiple = TRUE
            ),
            selectizeInput(
              "custom_x_unc",
              label = "x unc variables (optional, same order as x variables)",
              choices = character(0),
              multiple = TRUE
            ),
            checkboxInput("dirichlet", "Restrict (some) parameters to sum up to 1 (and larger than 0)"),
            conditionalPanel(
              condition = "input.dirichlet == true",
            textInput(
              "parRestricted",
              "To be restricted parameters names seperated by comma:",
              "a,b"
            )
            )
          ),
          # pickerInput(
          #   inputId = "f_yunc",
          #   label = "y-uncertainty (sd) - optional:",
          #   choices = character(0),
          #   options = list(
          #     "actions-box" = FALSE,
          #     "none-selected-text" = 'No variables selected',
          #     "max-options" = 1
          #   ),
          #   multiple = FALSE
          # ),
          # pickerInput(
          #   inputId = "f_xunc",
          #   label = "x-uncertainty (sd) - optional:",
          #   choices = character(0),
          #   options = list(
          #     "actions-box" = FALSE,
          #     "none-selected-text" = 'No variables selected',
          #     "max-options" = 1
          #   ),
          #   multiple = FALSE
          # ),
          sliderInput(
            inputId = "iter",
            label = "Number of total iterations",
            min = 500,
            max = 10000,
            step = 100,
            value = 2000
          ),
          sliderInput(
            inputId = "burnin",
            label = "Number of burnin iterations",
            min = 200,
            max = 3000,
            step = 100,
            value = 500
          ),
          sliderInput(
            inputId = "chains",
            label = "Number of MCMC chains",
            min = 1,
            max = 16,
            step = 1,
            value = 4
          ),
          conditionalPanel(
            condition = "input.selectFType == 'custom'",
            sliderInput(
              inputId = "thinning",
              label = "Thinning of MCMC chains",
              min = 1,
              max = 20,
              step = 1,
              value = 5
            )
          ),
          actionButton("saveFormula", "Define Formula")
        ),
        mainPanel(
          ## main panel ----
          tabsetPanel(
          id = "formTabs",
          tabPanel(
            "Defined Formulas",
            tableOutput('formTable') %>%
              withSpinner(color = "#20c997")
          ),
          tabPanel(
            "Convergence diagnostics",
            pickerInput("convF", "Choose formula", choices = character(0)),
            radioButtons(("diagType"),
                         label = "Diagnostics Type",
                         choices = c(
                           "Gelman Scale Reduction Factor" = "gelman",
                           "Raftery and Lewis" = "raftery",
                           "Geweke z-Score" = "geweke",
                           "Heidelberger-Welch" = "heidel"
                         )
            ),
            verbatimTextOutput(("diagnostics")),
            textExportButton(("exportText"))
          ),
          tabPanel(
            "Display formulas",
            pickerInput("dispF", "Choose formula", choices = character(0)),
            plotOutput("plotDisp"),
            selectInput("xVarDisp", "Choose x variable", choices = character(0)),
            fluidRow(
              column(4, shinyTools::plotTitlesUI("FormulasTitles")),
              column(4, shinyTools::plotRangesUI("FormulasRanges")),
              column(4,
                     tags$h4("Plot"),
            numericInput(
              inputId = ("xAxisSizeF"),
              label = "Font size x-axis",
              value = 18
            ),
            numericInput(
              inputId = ("yAxisSizeF"),
              label = "Font size y-axis",
              value = 18
            ),
            sliderInput(
              inputId = "PointSizeF",
              label = "Point size",
              min = 0.1, max = 5,value =  1, step = 0.1
            ),
            sliderInput(
              inputId = "LineWidthF",
              label = "Line Width",
              min = 0.1, max = 5,value =  1, step = 0.1
            )
            )),
            
            shinyTools::plotExportButton("exportPlotF", label = "Export Plot"),
            shinyTools::dataExportButton("exportDataF", label = "Export Data")
          )
        ))
      )
    ),
    # MEASURES -----------------------------------------------------------------------------------
    tabPanel(
      title = "Measures",
      id = "Measures",
      value = "Measures",
      sidebarLayout(
        sidebarPanel(
          ## left sidebar ----
          width = 2,
          style = "position:fixed; width:15%; max-width:350px; overflow-y:auto; height:85%",
          h3("Enter Data"),
          tags$br(),
          DataTools::importDataUI("MeasuresFile", "Import Data"),
          tags$br(), tags$br(),
          actionButton("simulateMeasures", "Load Example Data")
        ),
        mainPanel(
          ## main panel ----
          matrixInput(
            inputId = "measuresMatrix",
            class = "character",
            value = matrix(),
            copy = TRUE,
            paste = TRUE,
            cols = list(
              names = TRUE,
              extend = TRUE,
              delta = 1,
              editableNames = TRUE
            ),
            rows = list(
              names = FALSE,
              editableNames = TRUE,
              extend = TRUE,
              delta = 1
            )
          )
        ),
      )
    ),
    # ESTIMATES -----------------------------------------------------------------------------------
    tabPanel(
      title = "Estimates",
      id = "Estimates",
      value = "Estimates",
      fluidRow(
        sidebarPanel(
          ## left sidebar ----
          width = 2,
          style = "position:fixed; width:15%; max-width:350px; overflow-y:auto; height:85%",
          textInput("relationship", "relationship:", value = "Y ~ 3 + 4.5 * formula_1([X1])"),
          pickerInput(
            inputId = "regfunctions",
            label = "regfunctions:",
            choices = character(0),
            options = list("actions-box" = FALSE,
                           "none-selected-text" = 'No variables selected'),
            multiple = TRUE
          ),
          selectizeInput(
            inputId = "indVarsX",
            label = "indVars:",
            choices = character(0),
            multiple = TRUE
          ),
          selectizeInput(
            inputId = "indVarsUnc",
            label = "indVarsUnc:",
            choices = character(0),
            multiple = TRUE
          ),
          pickerInput(
            inputId = "category",
            label = "category:",
            choices = c("Category"),
            options = list("actions-box" = FALSE,
                           "none-selected-text" = 'No variables selected'),
            multiple = TRUE
          ),
          selectInput(
            "yDist",
            "Distribution of dependent variables:",
            choices = c("normal", "lognormal", "gamma"),
            selected = "normal"
          ),
          checkboxInput("imputeMissing",
                        "Impute missings in measurements (Multiple imputation)",
                        value = TRUE),
          checkboxInput("rangeRestrict",
                        "Restrict range of dependent variable:",
                        value = FALSE),
          conditionalPanel(
            condition = "input.rangeRestrict == true",
            numericInput("minRange", "Min restriction value (optional) ", value = NULL),
            numericInput("maxRange", "Max restriction value (optional) ", value = NULL)
          ),
          checkboxInput(
            "includeRegUnc",
            "Include uncertainty of regression parameters:",
            value = TRUE
          ),
          HTML("<br>"),
          actionButton("estimateY", "Estimate Y")#,
          # tags$hr(),
          # downloadModelUI("modelDownload", "Download Model"),
          # uploadModelUI("modelUpload", "Upload Model")
        ),
        mainPanel(
          ## main panel ----
          width = 8,
          selectInput(
            "summaryType",
            "Type",
            choices = c("Sample", "Combined", "Category"),
            selected = "Sample"
          ),
          tabsetPanel(
            id = "summaryTabs",
            tabPanel(
              ### Plot ----
              "Plot",
              value = "plotTab",
              conditionalPanel(
                condition = "input.summaryType == 'Category'",
                radioButtons(
                  "meanType",
                  label = "Type of category distribution",
                  choices = c(
                    "Mean distribution" = "1",
                    "Total variance distribution" = "2"
                  )
                )
              ),
              selectInput(
                "summaryPlotType",
                "Plot Type",
                choices = c("KernelDensity", "Histogram", "Boxplot"),
                selected = "KernelDensity"
              ),
              conditionalPanel(
                condition = "input.summaryPlotType == 'Histogram'",
                sliderInput(
                  "nBins",
                  label = "Number of histogram bins",
                  min = 3,
                  max = 500,
                  value = 50,
                  step = 1
                )
              ),
              
              plotOutput("plot") %>% withSpinner(color =
                                                   "#20c997"),
              fluidRow(
                column(4, shinyTools::plotTitlesUI("EstimateTitles")),
                column(4, shinyTools::plotRangesUI("EstimateRanges")),
              column(4,
                     tags$h4("Plot"),
              numericInput(
                inputId = ("xAxisSize"),
                label = "Font size x-axis",
                value = 18
              ),
              numericInput(
                inputId = ("yAxisSize"),
                label = "Font size y-axis",
                value = 18
              ),
              checkboxInput(
                inputId = ("showLegend"),
                label = "Show legend",
                value = TRUE
              ),
              conditionalPanel(
                condition = "input.summaryPlotType == 'Boxplot'",
              sliderInput(
                inputId = ("boxQuantile"),
                label = "Box upper quantile",
                value = 0.68,
                min = 0.5,
                max = 0.99,
                step = 0.01
              ),
              sliderInput(
                inputId = ("whiskerMultiplier"),
                label = "Whiskers coverage interval",
                value = 0.95,
                min = 0.5,
                max = 1,
                step = 0.001
              )),
              )),
              shinyTools::plotExportButton("exportPlot", label = "Export Plot"),
              shinyTools::dataExportButton("exportData", label = "Export Data")
            ),
            tabPanel(
              ### Summary Statistics ----
              "Summary Statistics",
              value = "summaryTab",
              tags$br(),
              fluidRow(
                column(
                  3,
                  numericInput(
                    "summaryProb",
                    label = "probability:",
                    value = 0.95,
                    min = 0,
                    max = 1,
                    step = 0.05
                  ),
                  tags$br(),
                  selectInput(
                    "summaryRefType",
                    "Reference Type:",
                    choices = c("none", "dist", "sample", "freqTable")
                  )
                ),
                column(
                  9,
                  conditionalPanel(condition = "input.summaryRefType == 'dist'",
                                   fluidRow(
                                     column(
                                       4,
                                       style = "margin-top: 6em;",
                                       selectInput(
                                         "summaryRefDist",
                                         "Reference Distribution:",
                                         choices = c(
                                           "Normal" = "norm",
                                           "Gamma" = "gamma",
                                           "Log-Normal" = "lnorm"
                                         )
                                       )
                                     ),
                                     column(
                                       4,
                                       style = "margin-top: 6em;",
                                       textInput("summaryRefParams",
                                                 "Reference Parameters:",
                                                 value = "50, 3")
                                     )
                                   )),
                  conditionalPanel(condition = "input.summaryRefType == 'sample'",
                                   fluidRow(
                                     column(
                                       4,
                                       style = "margin-top: 2em;",
                                       radioButtons(
                                         "refSampleSource",
                                         label = NULL,
                                         choices = c("Enter Data", "Upload Data")
                                       ),
                                       conditionalPanel(
                                         condition = "input.refSampleSource == 'Enter Data'",
                                         textInput("summaryRefSample",
                                                   "Reference Sample:",
                                                   value = "60, 52, 75, 48, 50, 56")
                                       ),
                                       conditionalPanel(
                                         condition = "input.refSampleSource == 'Upload Data'",
                                         tags$br(),
                                         DataTools::importDataUI("DataRefSample", "Import Reference Sample")
                                       )
                                     )
                                   )),
                  conditionalPanel(condition = "input.summaryRefType == 'freqTable'",
                                   fluidRow(
                                     column(
                                       4,
                                       style = "margin-top: 2em;",
                                       radioButtons(
                                         "refFreqTable",
                                         label = NULL,
                                         choices = c("Enter Data", "Upload Data")
                                       ),
                                       conditionalPanel(
                                         condition = "input.refFreqTable == 'Enter Data'",
                                         textInput("summaryFreqTable",
                                                   "Reference Values:",
                                                   value = "60, 52, 75, 48, 50, 56")
                                       ),
                                       conditionalPanel(
                                         condition = "input.refFreqTable == 'Upload Data'",
                                         tags$br(),
                                         DataTools::importDataUI("DataRefFreqTable", "Import Reference Values")
                                       )
                                     ),
                                     column(
                                       4,
                                       style = "margin-top: 6em;",
                                       conditionalPanel(
                                         condition = "input.refFreqTable == 'Enter Data'",
                                         textInput("summaryFreqTable2",
                                                   "Reference Frequencies:",
                                                   value = "1, 3, 6, 5, 4, 2")
                                       ),
                                       conditionalPanel(
                                         condition = "input.refFreqTable == 'Upload Data'",
                                         tags$br(),
                                         DataTools::importDataUI("DataRefFreqTable2", "Import Reference Frequencies")
                                       )
                                     )
                                   ))
                )
              ),
              tags$hr(),
              actionButton("estimateSummary", "Compute Summary Statistics"),
              tags$br(),
              tags$br(),
              verbatimTextOutput("summaryEstimates") %>%
                withSpinner(color = "#20c997"),
              shinyTools::dataExportButton("exportSummary", label = "Export Mean Tables")
            )
          ))
      )
    )
  ),
  #shinyTools::headerButtonsUI(id = "header", help_link = "https://pandora-isomemo.github.io/MapR/articles/how-to-use-MapR.html"),
  div(
    id = "header-right",
    div(
      id = "logo-mpi",
      tags$a(
        href = "https://www.mpg.de/en",
        img(src = "MPIlogo.png", alt = "Supported by the Max Planck society"),
        target = "_blank"
      )
    ),
    div(
      id = "logo-isomemo",
      tags$a(
        href = "https://isomemo.com/",
        img(src = "IsoMemoLogo.png", alt = "IsoMemo"),
        target = "_blank"
      )
    ),
    div(
      id = "further-help",
      tags$button(onclick = "window.open('https://isomemo.com','_blank');",
                  class = "btn btn-default",
                  "Further Help")
    ),
    div(id = "help",
        actionButton("getHelp", "?"))
  )
)
