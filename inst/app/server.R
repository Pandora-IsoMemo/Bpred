library(shiny)
library(shinyWidgets)
library(shinyMatrix)
library(dplyr)
library(ggplot2)
library(xlsx)
library(shinyjs)
library(mpiBpred)
library(coda)

options(shiny.maxRequestSize = 200*1024^2)

shinyServer(function(input, output, session) {
  # DATA -------------------------------------------------------------------------------
  data <- reactiveValues(dat = data.frame(), refSample = NULL, results = NULL, exportData = data.frame())
  
  output$data <- renderDataTable(data$dat)
  
  ### SIMULATE DATA 
  observeEvent(input$simulateData, {
    
    n <- eval(parse(text = input$n))
    x <- rnorm(n)
    x2 <- rnorm(n)
    y <- 1.5 + 2 * x + rnorm(n)
    xunc <- rep(0.25, length(x))
    yunc <- rep(0.25, length(y))
    xobs <- x + (rnorm(n, sd = xunc))
    yobs <- y + (rnorm(n, sd = yunc))
    y2 <- 2.5 - 0.2 * x + rnorm(n)
    yobs2 <- y + (rnorm(n, sd = yunc))
    
    data$dat <- cbind(x, y, xunc, yunc, xobs, yobs, y2, yobs2, x2) %>% 
      round(3) 
  })
  
  ### UPLOAD DATA 
  importedData <- DataTools::importDataServer(
    "DataFile",
    defaultSource = config()[["defaultSourceData"]],
    ckanFileTypes = config()[["ckanFileTypes"]],
    rPackageName = config()[["rPackageName"]],
    customErrorChecks = list(reactive(DataTools::checkAnyNonNumericColumns))
  )
  
  observeEvent(importedData(), {
    req(length(importedData()) > 0)
    data$dat <- importedData()[[1]]
  })
  
  observe({
    updatePickerInput(session, "f_y", choices = data$dat %>% colnames(), selected = (data$dat %>% colnames())[2])
    updatePickerInput(session, "f_x", choices = data$dat %>% colnames(), selected = (data$dat %>% colnames())[1])
    updateSelectInput(session, "f_yunc", choices = data$dat %>% colnames() %>% c("none", .), selected = "none")
    updateSelectInput(session, "f_xunc", choices = data$dat %>% colnames() %>% c("none", .), selected = "none")
    updateSelectizeInput(session, "custom_x", choices = data$dat %>% colnames() %>% c("none", .), selected = NULL)
    updateSelectizeInput(session, "custom_x_unc", choices = data$dat %>% colnames() %>% c("none", .), selected = NULL)
  })
  
  # FORMULAS --------------------------------------------------------------------
  formulas <- reactiveValues(f = data.frame(), objects = list())
  output$formTable <- renderTable(formulas$f)
  
  observeEvent(input$saveFormula, {
    if(!is.null(names(formulas$objects)) && input$formName %in% names(formulas$objects)){
      shinyjs::alert("Formula name already chosen")
      return(NULL)
    }
    
    # input checks
    req(nrow(data$dat) > 0)
    if(input$selectFType != "custom"){
      req(input$f_y %in% colnames(data$dat))
      req(input$f_x %in% colnames(data$dat))
      req(input$f_yunc == "none" || input$f_yunc %in% colnames(data$dat))
      req(input$f_xunc == "none" || input$f_xunc %in% colnames(data$dat))
    } else {
      req(grepl("\\[", input$formCustom) & grepl("\\{", input$formCustom))
      req(input$f_yunc == "none" || input$f_yunc %in% colnames(data$dat))
      req(input$f_y %in% colnames(data$dat))
      req(is.null(input$custom_x_unc) || input$custom_x_unc %in% colnames(data$dat))
      req(input$custom_x %in% colnames(data$dat))
    }
      if(input$selectFType != "custom"){
        X <- data$dat[, input$f_x, drop = FALSE]
        custom_x = input$f_x
        if(input$f_xunc == "none"){
          f_xunc <- NULL
        } else {
          f_xunc <- input$f_xunc
        }
        xUnc <- data$dat[, f_xunc, drop = FALSE]
        if(NCOL(xUnc) == 0){
          f_xunc <- "none"
        } else {
          f_xunc <- input$f_xunc
        }
        custom_x_unc <- f_xunc
        y <- data$dat[, input$f_y]
        if(input$f_yunc == "none"){
          yUnc <- NULL
        } else {
          yUnc <- data$dat[, input$f_yunc]
        }
        if(input$f_link == "linIntcp"){
          form <- paste0("{slope} * [",input$f_x,"] + {intercept}")
        }
        if(input$f_link == "linNoIntcp"){
          form <- paste0("{slope} * [",input$f_x,"]")
        }
        if(input$f_link == "sqrt"){
          form <- paste0("{slope} * sqrt([",input$f_x,"]) + {intercept}")
        }
        if(input$f_link == "log"){
          form <- paste0("{slope} * log([",input$f_x,"]) + {intercept}")
        }
        parNames <- gsub("[\\{\\}]", "", regmatches(form, gregexpr("\\{.*?\\}", form))[[1]])
        varNames <- gsub("\\[|\\]", "", regmatches(form, gregexpr("\\[.*?\\]", form))[[1]])
        parNamesDir <- NULL
      } else {
      X <- data$dat[, input$custom_x, drop = FALSE]
      xUnc <- data$dat[, input$custom_x_unc, drop = FALSE]
      if(NCOL(xUnc) == 0){
        custom_x_unc <- "none"
      } else {
        custom_x_unc <- input$custom_x_unc
      }
      y <- data$dat[, input$f_y]
      if(input$f_yunc == "none"){
        yUnc <- NULL
      } else {
        yUnc <- data$dat[, input$f_yunc]
      }
      form <- input$formCustom
      parNames <- gsub("[\\{\\}]", "", regmatches(form, gregexpr("\\{.*?\\}", form))[[1]])
      if(length(parNames) == 0){
        shinyjs::alert("Formula must have parameters in curly brackets.")
        return(NULL)
      }
      if(input$dirichlet == TRUE){
        parNamesDir <- strsplit(input$parRestricted, ",")[[1]]
        if(length(parNamesDir) == 0){
          shinyjs::alert("Formula must have parameters in curly brackets.")
          return(NULL)
        }
        
      } else {
        parNamesDir <- NULL
      }
      varNames <- gsub("\\[|\\]", "", regmatches(form, gregexpr("\\[.*?\\]", form))[[1]])
      if(!any(varNames %in% colnames(X))){
        shinyjs::alert("Variables in formula must be in data.")
        return(NULL)
      }
      set.seed(12345)
      }
       res <- fitModel(X, y, yUnc, xUnc, parNames, varNames, form,
                                        startPar = rep(0, length(parNames)),
                                        iter = input$iter,
                                        burnin = input$burnin,
                                        chains = input$chains,
                                        thinning = input$thinning,
                                        parNamesDir = parNamesDir) %>%
         DataTools::tryCatchWithWarningsAndErrors()
       
      if(class(res) == "character"){
        shinyjs::alert(res)
        return(NULL)
      }
      formulas$objects[[input$formName]] <- res
      
      form <- data.frame(name = input$formName,
                         y = input$f_y,
                         x = paste(input$custom_x, collapse = ", "),
                         yUnc = input$f_yunc,
                         xUnc = paste(custom_x_unc, collapse = ", "),
                         link = "none", 
                         formula = form) %>%
        enrichForm(parNames = parNames, formulasObject = formulas$objects[[input$formName]], y = y)
      
    formulas$f <- bind_rows(formulas$f, form)
    functionsFit(formulas)
})
  
  observe({
    updatePickerInput(session, "regfunctions", choices = formulas$objects %>% names())
    updatePickerInput(session, "convF", choices = formulas$objects %>% names())
    updatePickerInput(session, "dispF", choices = formulas$objects %>% names())
  })
  
  observe({
    updateSelectInput(session, "xVarDisp", choices = as.vector(sapply(formulas$objects, function(x) x$varNames)))
  })
  
  observeEvent(input$selectFType, {
    if(input$selectFType == "custom"){
      updateSliderInput(session, "iter", value = 5000, max = 500000)
      updateSliderInput(session, "burnin", value = 2000, max = 200000)
      updateSliderInput(session, "chains", value = 4)
    } else {
      updateSliderInput(session, "iter", value = 2000, max = 10000)
      updateSliderInput(session, "burnin", value = 500, max = 5000)
      updateSliderInput(session, "chains", value = 4)
    }
    })
  
  output$plotDisp <- renderPlot({
    req(data)
    if(!(input$xVarDisp == "")){
      withProgress({
        plotFunctions(data = data$dat, xVar = input$xVarDisp,
                      yVar = formulas$f[formulas$f == input$dispF, "y"],
                      obj = formulas$objects[[input$dispF]],
                      ylabel = input$ylabelF, xlabel = input$xlabelF, headerLabel = input$headerLabelF,
                      xTextSize = input$xTextSizeF, yTextSize = input$yTextSizeF,
                      xAxisSize = input$xAxisSizeF, yAxisSize = input$yAxisSizeF,
                      PointSize = input$PointSizeF, LineWidth = input$LineWidthF)$g
      },message = "Drawing plot")
    }
  })
  
  
  observeEvent(input$exportPlotF, {
    req(functionsFit())
    showModal(modalDialog(
      title = "Export Graphic",
      footer = modalButton("OK"),
      exportPlotPopUpUI("plotExportF"),
      easyClose = TRUE
    ))
  })
  exportPlotPopUpServer("plotExportF", 
                        exportPlot = reactive(
                          plotFunctions(data = data$dat, xVar = input$xVarDisp,
                                        yVar = functionsFit()$f[functionsFit()$f == input$dispF, "y"],
                                        obj = functionsFit()$objects[[input$dispF]],
                                        ylabel = input$ylabelF, xlabel = input$xlabelF, headerLabel = input$headerLabelF,
                                        xTextSize = input$xTextSizeF, yTextSize = input$yTextSizeF,
                                        xAxisSize = input$xAxisSizeF, yAxisSize = input$yAxisSizeF,
                                        PointSize = input$PointSizeF, LineWidth = input$LineWidthF)$g
                        ), 
                        filename = paste(gsub("-", "", Sys.Date()), "plotEstimates", sep = "_")
  )
  
  shinyTools::dataExportServer("exportDataF", 
                               dataFun = reactive(function() {
                                 plotFunctions(data = data$dat, xVar = input$xVarDisp,
                                               yVar = functionsFit()$f[functionsFit()$f == input$dispF, "y"],
                                               obj = functionsFit()$objects[[input$dispF]],
                                               ylabel = input$ylabelF, xlabel = input$xlabelF, headerLabel = input$headerLabelF,
                                               xTextSize = input$xTextSizeF, yTextSize = input$yTextSizeF,
                                               xAxisSize = input$xAxisSizeF, yAxisSize = input$yAxisSizeF,
                                               PointSize = input$PointSizeF, LineWidth = input$LineWidthF)$exportData
                               }), 
                               filename = paste(gsub("-", "", Sys.Date()), "formulasData", sep = "_"))
  
  #convergence diagnostics
  printFunDiag <- reactive({
    req(formulas$objects[[input$convF]])
      obj <- formulas$objects[[input$convF]]
      parameters <- obj$beta
      nChains <- obj$nChains
      colnames(parameters) <- obj$parNames
      parameters <- as.data.frame(parameters)
      diag <- convergenceDiagnostics(parameters, nChains)
      diagType <- input$diagType
      return(diag[[diagType]])
  })
  
  output$diagnostics <- renderPrint({
    req(formulas$objects[[input$convF]])
    print(printFunDiag())
  })
  callModule(textExport, "exportText", printFun = printFunDiag, filename = "diagnostics")
  
  # MEASURES --------------------------------------------------------------------

  #output$measures <- renderDataTable(data$measures)
  
  observeEvent(input$simulateMeasures, {
    newMeasures <- data.frame(Category = c("Site1", "Site1", NA, "Site2", "Site2"),
                              X1 = c(1, 0.9, 1.2, 4, 5),
                              SD_X1 = c(0.2, 0.3, 0.2, 0.2, 0.3),
                              X2 = c(1.5, 1.8, 1.1, 2.25, NA),
                              SD_X2 = c(0.5, 0.3, 0.2, 0.2, 0.3))
    
    updateMatrixInput(session, "measuresMatrix", value = newMeasures %>% as.matrix())
  })
  
  importedMeasures <- DataTools::importDataServer(
    "MeasuresFile",
    defaultSource = config()[["defaultSourceData"]],
    ckanFileTypes = config()[["ckanFileTypes"]],
    rPackageName = config()[["rPackageName"]],
    ignoreWarnings = TRUE
    #customErrorChecks = list(reactive(DataTools::checkAnyNonNumericColumns))
  )
  
  observeEvent(importedMeasures(), {
    req(length(importedMeasures()) > 0)
    newMeasures <- importedMeasures()[[1]]
    
    if (length(newMeasures) == 0) {
      newVal <- structure("", dim = c(1L, 1L), dimnames = list("", ""))
    } else {
      newVal <- newMeasures %>% 
        as.matrix()
    }
    
    req(!identical(input$measuresMatrix, newVal))
    updateMatrixInput(session, "measuresMatrix", value = newVal)
  })
  
  observeEvent(input$measuresMatrix, {
    req(!identical(data$measures, measureMatrixToDf(input$measuresMatrix)))
    
    if (ncol(measureMatrixToDf(input$measuresMatrix)) == 0) {
      # reset
      data$measures <- NULL
    } else {
      data$measures <- measureMatrixToDf(input$measuresMatrix)
    }
  })
  
  observeEvent(data$measures, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (length(data$measures) == 0) {
      # reset
      newChoices <- character(0)
    } else {
      newChoices <- data$measures %>% colnames()
    }
    
    updateSelectizeInput(session, "indVarsX", choices = newChoices) 
    updateSelectizeInput(session, "indVarsUnc", choices = newChoices)
    updatePickerInput(session, "category", choices = newChoices)
  })

  observe({
    if(input$summaryType == "Category"){
      updateSelectInput(session, "summaryPlotType",choices = c("KernelDensity", "Histogram", "Boxplot", "TimePlot"))
    } else {
      updateSelectInput(session, "summaryPlotType",choices = c("KernelDensity", "Histogram", "Boxplot"))
    }
  })
  
  # ESTIMATES -------------------------------------------------------------------
  yEstimates <- reactiveVal(NULL)
  functionsFit <- reactiveVal(NULL)
  observeEvent(input$estimateY, {

    # if(!all(sapply(c(
    #   length(paste0("c(", paste0("'", input$indVarsX, "'", collapse = ", "), ")") %>% parse(text = .) %>% eval),
    #   length(paste0("c(", paste0("'", input$indVarsUnc, "'", collapse = ", "), ")") %>% parse(text = .) %>% eval)), 
    #   FUN = identical, length(paste0("c(", paste0("'", input$regfunctions, "'", collapse = ", "), ")") %>% parse(text = .) %>% eval ))))
    #   { 
    #     shinyjs::alert("For 'regfunctions', 'indVars' and 'indVarsUnc' always the same amount of variables has to be selected.")
    #     return()
    # }
    lInd <- length(which((paste0("c(", paste0("'", input$indVarsX, "'", collapse = ", "), ")") %>% parse(text = .) %>% eval) != ""))
    if(is.null(input$indVarsUnc)){
      lUnc <- 0
    } else {
      lUnc <- length(which((paste0("c(", paste0("'", input$indVarsUnc, "'", collapse = ", "), ")") %>% parse(text = .) %>% eval) != ""))
    }
    lregFunc <- length(which((paste0("c(", paste0("'", input$regfunctions, "'", collapse = ", "), ")") %>% parse(text = .) %>% eval) != ""))
    
    if(length(gregexpr("\\[", input$relationship)[[1]]) < lInd | length(gregexpr("\\]", input$relationship)[[1]]) < lInd){
      shinyjs::alert("Reminder: Variable names in relationship formula must be surrounded by brackets.")
    }
    if(lUnc > 0 & lInd != lUnc){
      shinyjs::alert("For 'indVars' and 'indVarsUnc' have to have the same amount of variables has to be selected if any uncertainty variables were selected.")
      return()
    }
    
    if(lInd == 0){
      shinyjs::alert("Please specify independent variables")
      return()
    }
    
    # if(is.null(input$category)){
    #   shinyjs::alert("Please define a category variable.") 
    #   return()
    # }
    
if(is.null(input$regfunctions)){
  regfunctions <- paste0("list()")
} else {
  regfunctions <- paste0("list(",
                         paste0(input$regfunctions,
                                " = formulas$objects[['",input$regfunctions,"']]",
                                collapse = ", "), ")")
}
    model <- withProgress({paste0("estimateY(relationship = '", input$relationship, "', ", 
           "regfunctions = ", regfunctions, ", ",
           "indVars= c(", paste0("'", input$indVarsX, "'", collapse = ", "),"), ",
           "data = data$measures,", 
           "indVarsUnc = c(", paste0("'", input$indVarsUnc, "'", collapse = ", "),"), ",
           "category = '", input$category, "', ",
           "n_samples = ", input$n_samples, ", ",
           "includeRegUnc = ", input$includeRegUnc, ", ",
           "rangeRestrict = ", input$rangeRestrict, ", ",
           "rangeY = c(", input$minRange,",", input$maxRange,") ", ", ",
           "distribution = '", input$yDist, "', ",
           "imputeMissing = ", input$imputeMissing,")") %>% 
      parse(text = .) %>%
      eval() %>%
        DataTools::tryCatchWithWarningsAndErrors()},
      message = "computing new y estimates", value = 0.3)
    if(class(model) == "character"){
      shinyjs::alert(model) 
      return()
    }
    yEstimates(model)
  })
  
  observeEvent(input$estimateY | input$estimateSummary, {
    req(yEstimates())
    req(input$summaryType)
    req(input$summaryProb)
    req(input$summaryRefType)
    req(input$summaryRefParams)
    req(data$refSample)
    req(data$values)
    req(data$freq)
    data$results <- withProgress({summariseEstimates(yEstimates(),
                       type = input$summaryType,
                       probability = as.numeric(gsub(",", ".", input$summaryProb)),
                       checkDifferencesReference = !(input$summaryRefType == "none"),
                       referenceType = input$summaryRefType,
                       referenceDist = input$summaryRefDist,
                       referenceParameters = paste0("c(", paste0(input$summaryRefParams, ")", collapse = ", ")) %>% parse(text = .),
                       referenceSample = eval(data$refSample),
                       referenceTable = matrix(c(eval(data$values), 
                                                 eval(data$freq)), nrow = 2),
                       meanType = input$meanType)},
                       message = "computing summary estimates", value = 0.7)
  })
  
  ## Enter refSample ----
  observeEvent(input$summaryRefSample, {
    data$refSample <- paste0("c(", input$summaryRefSample, ")") %>% parse(text = .)
  })
  
  importedRefSample <- DataTools::importDataServer("DataRefSample", 
                                                   defaultSource = config()[["defaultSourceData"]],
                                                   ckanFileTypes = config()[["ckanFileTypes"]],
                                                   rPackageName = config()[["rPackageName"]])
  observeEvent(importedRefSample(), {
    req(length(importedRefSample()) > 0)
    data$refSample <- importedRefSample()[[1]]
    alert("Reference Sample updated.")
  })

  ## Enter values & freq ----
  observeEvent(input$summaryFreqTable, {
    data$values <- paste0("c(", input$summaryFreqTable, ")") %>% parse(text = .)
  })
  observeEvent(input$summaryFreqTable2, {
    data$freq <- paste0("c(", input$summaryFreqTable2, ")") %>% parse(text = .)
  })
  
  importedRefFreqTable <- DataTools::importDataServer("DataRefFreqTable", 
                                                      defaultSource = config()[["defaultSourceData"]],
                                                      ckanFileTypes = config()[["ckanFileTypes"]],
                                                      rPackageName = config()[["rPackageName"]])
  observeEvent(importedRefFreqTable(), {
    req(length(importedRefFreqTable()) > 0)
    data$values <- importedRefFreqTable()[[1]]
    alert("Reference Values updated.")
  })
  
  importedRefFreqTable2 <- DataTools::importDataServer("DataRefFreqTable2", 
                                                       defaultSource = config()[["defaultSourceData"]],
                                                       ckanFileTypes = config()[["ckanFileTypes"]],
                                                       rPackageName = config()[["rPackageName"]])
  observeEvent(importedRefFreqTable2(), {
    req(length(importedRefFreqTable2()) > 0)
    data$freq <- importedRefFreqTable2()[[1]]
    alert("Reference frequencies updated.")
  })
  
  observe({
    req(yEstimates())
    yEstimates <- yEstimates()
    
    if (input$summaryType == "Sample"){
      data$exportData <-
        data.frame(Value = unlist(yEstimates$Y_Samples_Individual),
                   Individual =
                     factor(rep(1:length(yEstimates$Y_Samples_Individual),
                                times = unlist(lapply(yEstimates$Y_Samples_Individual, length)))))
    }
    
    if (input$summaryType == "Combined"){
      data$exportData <- data.frame(Value = unlist(yEstimates$Y_Samples_Combined))
    }
    if (input$summaryType == "Category"){
      if (is.null(yEstimates$Y_Samples_Category)){
        alert("No categories found. Check your data")
        return()
      }
      if(input$meanType == "1"){
        export <- yEstimates$Y_Samples_Category_Mean
      } else {
        export <- yEstimates$Y_Samples_Category
      }
      data$exportData <-
        data.frame(Value = unlist(export),
                   Category = factor(rep(names(export),
                                         times = unlist(lapply(export, length)))))
    } 
  })
  

  output$summaryEstimates <- renderPrint({
    req(data$results)
    data$results
    })  
  
  plotEstimatesTitles <- shinyTools::plotTitlesServer(
    "EstimateTitles", 
    type = "ggplot",
    initTitles = list(plot = config()[["plotTitle"]],
                      xAxis = config()[["plotTitle"]],
                      yAxis = config()[["plotTitle"]])
  )
  
  plotEstimatesRanges <- shinyTools::plotRangesServer(
    "EstimateRanges",
    type = "ggplot",
    initRanges = list(xAxis = config()[["plotRange"]],
                      yAxis = config()[["plotRange"]])
  )
  
  output$plot <- renderPlot({
    req(yEstimates())
    plotDensities(yEstimates(), type = input$summaryType, plotType = input$summaryPlotType, nBins = input$nBins, meanType = input$meanType,
                  xAxisSize = input$xAxisSize, yAxisSize = input$yAxisSize,
                  showLegend = input$showLegend,
                  #colorPalette = "default",
                  #fontFamily = NULL,
                  whiskerMultiplier = input$whiskerMultiplier,
                  boxQuantile = input$boxQuantile) %>%
      shinyTools::formatTitlesOfGGplot(titles = plotEstimatesTitles) %>%
      shinyTools::formatRangesOfGGplot(ranges = plotEstimatesRanges)
  })
  
  shinyTools::dataExportServer("exportSummary", 
                               dataFun = reactive(function() { data$results[1] }), 
                               filename = paste(gsub("-", "", Sys.Date()), "Summary", sep = "_"))
  
  shinyTools::plotExportServer("exportPlot",
                               plotFun = reactive(function() {
                                 plotDensities(yEstimates(),
                                               type = input$summaryType, 
                                               plotType = input$summaryPlotType, 
                                               nBins = input$nBins,
                                               meanType = input$meanType,
                                               xAxisSize = input$xAxisSize, yAxisSize = input$yAxisSize,
                                               showLegend = input$showLegend,
                                               #colorPalette = "default",
                                               #fontFamily = NULL,
                                               whiskerMultiplier = input$whiskerMultiplier,
                                               boxQuantile = input$boxQuantile)
                               }),
                               plotType = "ggplot",
                               filename = paste(gsub("-", "", Sys.Date()), "plotEstimates", sep = "_"),
                               initTitles = plotEstimatesTitles,
                               initRanges = plotEstimatesRanges)
  
  shinyTools::dataExportServer("exportData", 
                               dataFun = reactive(function() {data$exportData}), 
                               filename = paste(gsub("-", "", Sys.Date()), "yEstimatesData", sep = "_"))
  
  # MODEL DOWN- / UPLOAD ----
  
  uploadedNotes <- reactiveVal(NULL)
  DataTools::downloadModelServer("modelDownload",
                                 dat = reactive(reactiveValuesToList(data)),
                                 inputs = reactiveValues(inputObj = reactiveValuesToList(input),
                                                         formulasObj = reactiveValuesToList(formulas)),
                                 model = yEstimates,
                                 rPackageName = config()[["rPackageName"]],
                                 fileExtension = config()[["fileExtension"]],
                                 modelNotes = uploadedNotes,
                                 triggerUpdate = reactive(TRUE))

  uploadedValues <- DataTools::importDataServer("modelUpload",
                                                title = "Import Model",
                                                importType = "model",
                                                ckanFileTypes = config()[["ckanModelTypes"]],
                                                ignoreWarnings = TRUE,
                                                defaultSource = config()[["defaultSourceModel"]],
                                                mainFolder = config()[["mainFolder"]],
                                                fileExtension = config()[["fileExtension"]],
                                                rPackageName = config()[["rPackageName"]])
  
  observe({
    req(length(uploadedValues()) > 0, !is.null(uploadedValues()[[1]][["data"]]))
    
    # update data object in tab "Data" ----
    uploadedData <- uploadedValues()[[1]][["data"]]
    for (name in names(data))
      if (!is.null(uploadedData[[name]])) {
        data[[name]] <- uploadedData[[name]]
      } else if (name %in% c("dat", "exportedData")) {
        data[[name]] <- data.frame()
      } else {
        data[[name]] <- NULL
      }
    
    # update notes in tab "Estimates" model download ----
    uploadedNotes(uploadedValues()[[1]][["notes"]])
  }) %>% 
    bindEvent(uploadedValues())
  
  observe(priority = -100, {
    req(length(uploadedValues()) > 0, !is.null(uploadedValues()[[1]][["inputs"]]))
    # update data in "Defined Formulas" in tab "Formulas" ----
    uploadedFormulas <- uploadedValues()[[1]][["inputs"]][["formulasObj"]]
    for (name in names(formulas))
      if (!is.null(uploadedFormulas[[name]])) {
        formulas[[name]] <- uploadedFormulas[[name]]
      } else if (name == "f") {
        formulas$f <- data.frame()
      } else {
        formulas[[name]] <- list()
      }
    
    if (!is.null(uploadedValues()[[1]][["inputs"]][["inputObj"]][["measuresMatrix"]])) {
      # update data in tab "Measures" ----
      uploadedMeasures <- uploadedValues()[[1]][["inputs"]][["inputObj"]][["measuresMatrix"]] %>% 
        as.matrix()
      # session$sendInputMessage() does not work for matrixInput()
      updateMatrixInput(session, "measuresMatrix", value = uploadedMeasures)
    }
  }) %>% 
    bindEvent(uploadedValues())
  
  observe(priority = -200, {
    req(length(uploadedValues()) > 0, !is.null(uploadedValues()[[1]][["inputs"]]))
    ## update inputs ----
    uploadedInputs <- uploadedValues()[[1]][["inputs"]][["inputObj"]]
    
    # following inputs are updated differently
    excludedInputs <- c("measuresMatrix", "indVars", "indVarsX", "indVarsUnc", "category", "yDist",
                        "n_samples", "includeRegUnc")
    
    inputIDs <- names(uploadedInputs)
    inputIDs <- inputIDs[(inputIDs %in% names(input)) & !(inputIDs %in% excludedInputs)]
    
    for (i in 1:length(inputIDs)) {
      session$sendInputMessage(inputIDs[i],  list(value = uploadedInputs[[inputIDs[i]]]) )
    }
    
    # update model tab "Estimates" ----
    uploadedModel <- uploadedValues()[[1]][["model"]]
    
    ## update these inputs from model output
    # because these inputs are also available for formally saved model objects, before version 22.11.1)
    
    # rename to remove name ambiguities
    if (!is.null(uploadedModel[["indVars"]])) {
      uploadedModel[["indVarsX"]] <- uploadedModel[["indVars"]]
      uploadedModel[["indVars"]] <- NULL
    }
    
    inputIDs <- c("indVarsX", "indVarsUnc", "category", "n_samples", "includeRegUnc")
    inputIDs <- inputIDs[(inputIDs %in% names(input))]
    
    for (i in 1:length(inputIDs)) {
      session$sendInputMessage(inputIDs[i],  list(value = uploadedModel[[inputIDs[i]]]) )
    }
    updateSelectInput(session, "yDist", selected = uploadedModel[["distribution"]])
    
    ## update model object
    yEstimates(uploadedModel)
  }) %>% 
    bindEvent(uploadedValues())
    
  observeEvent(input$getHelp, {
    showModal(modalDialog(
      title = "Help",
      easyClose = TRUE,
      getHelp(input$tab)
    ))
  })
})
