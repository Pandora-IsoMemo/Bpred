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
  importedData <- importDataServer(
    "DataFile",
    defaultSource = "file",
    customErrorChecks = list(reactive(checkAnyNonNumericColumns))
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
                                        parNamesDir = parNamesDir)
       
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
                         formula = form,
                         parameter = paste(parNames,
                                           signif(colMeans(formulas$objects[[input$formName]]$beta), 4), sep = " = ", collapse = ", "),
                         credible_intervals_95p = paste0(lapply(1:length(parNames),
                                                                  function(x) paste(parNames[x],
                                                                                    paste(signif(quantile(formulas$objects[[input$formName]]$beta[,x],c(0.025, 0.975)), 4), collapse = ","),
                                                                                                             sep = " = (", collapse = ",)")), collapse = ", ", ")"),
                         R_squared = signif(1 - sum((colMeans(res$yPredmc) - y)^2) / sum((y-mean(y))^2), 4),
                         p_direction = paste0(lapply(1:length(parNames),
                                                     function(x) paste(parNames[x],
                                                                       paste(signif(max(sum(formulas$objects[[input$formName]]$beta[,x] > 0),
                                                                                        sum(formulas$objects[[input$formName]]$beta[,x] < 0)) / length(formulas$objects[[input$formName]]$beta[,x]), 4), collapse = ","),
                                                                       sep = " = (", collapse = "")), collapse = ", ", ")"))
    formulas$f <- data.frame(rbind(formulas$f, form))
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
  
  
  observeEvent(input$exportDataF, {
    req(functionsFit())
    showModal(modalDialog(
      "Export Data",
      easyClose = TRUE,
      footer = modalButton("OK"),
      exportPopUpUI("dataExportF")
    ))
  })
  exportPopUpServer("dataExportF",
                    exportData = reactive(
                      plotFunctions(data = data$dat, xVar = input$xVarDisp,
                                    yVar = functionsFit()$f[functionsFit()$f == input$dispF, "y"],
                                    obj = functionsFit()$objects[[input$dispF]],
                                    ylabel = input$ylabelF, xlabel = input$xlabelF, headerLabel = input$headerLabelF,
                                    xTextSize = input$xTextSizeF, yTextSize = input$yTextSizeF,
                                    xAxisSize = input$xAxisSizeF, yAxisSize = input$yAxisSizeF,
                                    PointSize = input$PointSizeF, LineWidth = input$LineWidthF)$exportData
                    ), 
                    filename = paste(gsub("-", "", Sys.Date()), "yEstimatesData", sep = "_"))
  
  
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
    data$measures <- data.frame(Category = c("Site1", "Site1", NA, "Site2", "Site2"),
                            X1 = c(1, 0.9, 1.2, 4, 5),
                            SD_X1 = c(0.2, 0.3, 0.2, 0.2, 0.3),
                            X2 = c(1.5, 1.8, 1.1, 2.25, NA),
                            SD_X2 = c(0.5, 0.3, 0.2, 0.2, 0.3))
  })
  
  importedMeasures <- importDataServer(
    "MeasuresFile",
    defaultSource = "file")
    #customErrorChecks = list(reactive(checkAnyNonNumericColumns)))
  
  observeEvent(importedMeasures(), {
    req(length(importedMeasures()) > 0)
    data$measures <- importedMeasures()[[1]]
  })
  
  observeEvent(data$measures, {
    req(data$measures)
    updateMatrixInput(session, "measuresMatrix", value = data$measures %>% as.matrix())
  })
  
  observe({
    updateSelectizeInput(session, "indVars", choices = data$measures %>% colnames()) 
    updateSelectizeInput(session, "indVarsUnc", choices = data$measures %>% colnames())
    updatePickerInput(session, "category", choices = data$measures %>% colnames()) 
    
  })

  observe({
    if(input$summaryType == "Category"){
      updateSelectInput(session, "summaryPlotType",choices = c("KernelDensity", "Histogram", "Boxplot", "TimePlot"))
    } else {
      updateSelectInput(session, "summaryPlotType",choices = c("KernelDensity", "Histogram", "Boxplot"))
    }

  })
  
  
  observeEvent(input$measuresMatrix, {
    data$measures <- measureMatrixToDf(input$measuresMatrix)
  })

  # ESTIMATES -------------------------------------------------------------------
  yEstimates <- reactiveVal(NULL)
  functionsFit <- reactiveVal(NULL)
  observeEvent(input$estimateY, {

    # if(!all(sapply(c(
    #   length(paste0("c(", paste0("'", input$indVars, "'", collapse = ", "), ")") %>% parse(text = .) %>% eval),
    #   length(paste0("c(", paste0("'", input$indVarsUnc, "'", collapse = ", "), ")") %>% parse(text = .) %>% eval)), 
    #   FUN = identical, length(paste0("c(", paste0("'", input$regfunctions, "'", collapse = ", "), ")") %>% parse(text = .) %>% eval ))))
    #   { 
    #     shinyjs::alert("For 'regfunctions', 'indVars' and 'indVarsUnc' always the same amount of variables has to be selected.")
    #     return()
    # }
    lInd <- length(which((paste0("c(", paste0("'", input$indVars, "'", collapse = ", "), ")") %>% parse(text = .) %>% eval) != ""))
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
           "indVars= c(", paste0("'", input$indVars, "'", collapse = ", "),"), ",
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
      eval},
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
  
  
  observeEvent(input$DataRefSample, {
    
    inFile <- input$DataRefSample
    if (is.null(input$DataRefSample)) return(NULL)
    file <- inFile$datapath

    ref <- try({
      if(grepl(".csv$", file)){
        read.csv(file, sep = input$colseparatorData, dec = input$decseparatorData)
      } else if(grepl(".xlsx$", file)){
        read.xlsx(file, sheetIndex = 1)
      }
    })
    if (inherits(ref, "try-error")) {
      alert("Could not read in file")
      return()
    }
    
    data$refSample <- ref
  })
  
  observeEvent(input$summaryRefSample, {
    data$refSample <- paste0("c(", input$summaryRefSample, ")") %>% parse(text = .)
  })

  observeEvent(input$summaryFreqTable, {
    data$values <- paste0("c(", input$summaryFreqTable, ")") %>% parse(text = .)
  })
  observeEvent(input$summaryFreqTable2, {
    data$freq <- paste0("c(", input$summaryFreqTable2, ")") %>% parse(text = .)
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
  
  output$plot <- renderPlot({
    req(yEstimates())
    plotDensities(yEstimates(), type = input$summaryType, plotType = input$summaryPlotType, nBins = input$nBins, meanType = input$meanType,
                  ylabel = input$ylabel, xlabel = input$xlabel, headerLabel = input$headerLabel,
                  xTextSize = input$xTextSize, yTextSize = input$yTextSize,
                  xAxisSize = input$xAxisSize, yAxisSize = input$yAxisSize,
                  showLegend = input$showLegend,
                  #colorPalette = "default",
                  #fontFamily = NULL,
                  whiskerMultiplier = input$whiskerMultiplier,
                  boxQuantile = input$boxQuantile)
  })
  
  
  observeEvent(input$exportSummary, {
    req(data$results[1])
    showModal(modalDialog(
      "Export Data",
      easyClose = TRUE,
      footer = modalButton("OK"),
      exportPopUpUI("summaryExport")
    ))
  })
  exportPopUpServer("summaryExport",
                    exportData = reactive(data$results[1]), 
                    filename = paste(gsub("-", "", Sys.Date()), "Summary", sep = "_"))
  
  
  observeEvent(input$exportPlot, {
    req(yEstimates())
    showModal(modalDialog(
      title = "Export Graphic",
      footer = modalButton("OK"),
      exportPlotPopUpUI("plotExport"),
      easyClose = TRUE
    ))
  })
  exportPlotPopUpServer("plotExport", 
                        exportPlot = reactive(
                          plotDensities(yEstimates(),
                                        type = input$summaryType, 
                                        plotType = input$summaryPlotType, 
                                        nBins = input$nBins,
                                        meanType = input$meanType,
                                        ylabel = input$ylabel, xlabel = input$xlabel, headerLabel = input$headerLabel,
                                        xTextSize = input$xTextSize, yTextSize = input$yTextSize,
                                        xAxisSize = input$xAxisSize, yAxisSize = input$yAxisSize,
                                        showLegend = input$showLegend,
                                        #colorPalette = "default",
                                        #fontFamily = NULL,
                                        whiskerMultiplier = input$whiskerMultiplier,
                                        boxQuantile = input$boxQuantile)
                        ), 
                        filename = paste(gsub("-", "", Sys.Date()), "plotEstimates", sep = "_")
  )

  
  observeEvent(input$exportData, {
    req(data$exportData)
    showModal(modalDialog(
      "Export Data",
      easyClose = TRUE,
      footer = modalButton("OK"),
      exportPopUpUI("dataExport")
    ))
  })
  exportPopUpServer("dataExport",
                    exportData = reactive(data$exportData), 
                    filename = paste(gsub("-", "", Sys.Date()), "yEstimatesData", sep = "_"))
  
  
  # MODEL DOWN- / UPLOAD ----
  
  uploadedNotes <- reactiveVal()
  callModule(downloadModel, "modelDownload", 
             allParentInput = reactive(reactiveValuesToList(input)),
             yEstimates = yEstimates, formulas = formulas, data = data, 
             uploadedNotes = uploadedNotes)

  uploadedData <- callModule(uploadModel, "modelUpload")
  
  observeEvent(uploadedData$data, {
    # update data in tab "Data" and tab "Measures" ----
    for (name in names(data))
      if (!is.null(uploadedData$data[[name]])) {
        data[[name]] <- uploadedData$data[[name]]
      } else if (name %in% c("dat", "exportedData")) {
        data[[name]] <- data.frame()
      } else {
        data[[name]] <- NULL
      }
  })

  observeEvent(uploadedData$formulas, {
    # update data in "Defined Formulas" in tab "Formulas" ----
    for (name in names(formulas))
      if (!is.null(uploadedData$formulas[[name]])) {
        formulas[[name]] <- uploadedData$formulas[[name]]
      } else if (name == "f") {
        formulas$f <- data.frame()
      } else {
        formulas[[name]] <- list()
      }
  })
  
  observeEvent(uploadedData$notes, {
    # update model and notes in tab "Estimates" ----
    uploadedNotes(uploadedData$notes)
  })
  
  observeEvent(uploadedData$model, priority = -100, {
    # update model and notes in tab "Estimates" ----
    yEstimates(uploadedData$model)
    
    ## update these inputs from model output ----
    #(this is also available for formally saved model objects, before version 22.11.1)
    updateSelectizeInput(session, "indVars", selected = uploadedData$model$indVars)
    updateSelectizeInput(session, "indVarsUnc", selected = uploadedData$model$indVarsUnc)
    updatePickerInput(session, "category", selected = uploadedData$model$category)
    updateSelectInput(session, "yDist", selected = uploadedData$model$distribution)
    updateTextInput(session, "n_samples", value = uploadedData$model$n_samples)
    updateCheckboxInput(session, "includeRegUnc", value = uploadedData$model$includeRegUnc)
  })
  
  observeEvent(uploadedData$inputFields, priority = -100, {
    inputFields <- uploadedData$inputFields
    # update inputs in tab "DATA" ----
    updateNumericInput(session, "n", value = inputFields[["n"]])
    
    # update inputs in tab "FORMULAS" and "Estimates" ----
    ## updateTextInput
    for (i in c("formName", "formCustom", "parRestricted", "relationship")) {
      if (!is.null(i)) {
        updateTextInput(session, i, value = inputFields[[i]])
      } else {
        updateTextInput(session, i, value = "")
      }
    }
    ## updatePickerInput
    for (i in c("f_y", "f_x", "f_xunc", "regfunctions")) {
      if (!is.null(i)) {
        updatePickerInput(session, i, selected = inputFields[[i]])
      } else {
        updatePickerInput(session, i, selected = list())
      }
    }
    ## updateSelectInput
    for (i in c("f_yunc", "f_xunc", "f_link")) {
      if (!is.null(i)) {
        updateSelectInput(session, i, selected = inputFields[[i]])
      } else {
        updateSelectInput(session, i, selected = list())
      }
    }
    
    ## updateRadioButtons
    if (!is.null(inputFields[["selectFType"]])) {
      updateRadioButtons(session, "selectFType", selected = inputFields[["selectFType"]])
    } else {
      updateRadioButtons(session, "selectFType", selected = character(0))
    }
    
    ## updateSelectizeInput
    for (i in c("custom_x", "custom_x_unc")) {
      if (!is.null(i)) {
        updateSelectizeInput(session, i, selected = inputFields[[i]])
      } else {
        updateSelectizeInput(session, i, selected = list())
      }
    }
    
    ## updateCheckboxInput
    for (i in c("dirichlet", "imputeMissing", "rangeRestrict")) {
      if (!is.null(i)) {
        updateCheckboxInput(session, i, value = inputFields[[i]])
      } else {
        updateCheckboxInput(session, i, value = FALSE)
      }
    }
    
    ## updateSliderInput, updateNumericInput
    # if is.null, than no update (is.null is ignored by default):
    updateSliderInput(session, "iter", value = inputFields[["iter"]])
    updateSliderInput(session, "burnin", value = inputFields[["burnin"]])
    updateSliderInput(session, "chains", value = inputFields[["chains"]])
    updateSliderInput(session, "thinning", value = inputFields[["thinning"]])
    updateNumericInput(session, "minRange", value = inputFields[["minRange"]])
    updateNumericInput(session, "maxRange", value = inputFields[["maxRange"]])
  })
    
  observeEvent(input$getHelp, {
    showModal(modalDialog(
      title = "Help",
      easyClose = TRUE,
      getHelp(input$tab)
    ))
  })
})
