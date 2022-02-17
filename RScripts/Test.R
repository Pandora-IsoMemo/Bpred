# library(mpiBpred)
# set.seed(1345234)
# 
# #create simulated data set
# n <- 100
# y <- rnorm(n)
# x <- rnorm(n)
# y <- 1.5 + 2 * x + rnorm(n)
# xunc <- rep(0.25, length(x))
# yunc <- rep(0.25, length(y))
# xobs <- x + rnorm(n, sd = xunc)
# yobs <- y + rnorm(n, sd = yunc)
# 
# #second formula
# y <- 2.5 - 0.2 * x + rnorm(n)
# yobs2 <- y + rnorm(n, sd = yunc)
# #estimate formulas
# f1 <- estimateRelationship(y = yobs, x = xobs, yunc = yunc, xunc = xunc, link = "linIntcp")
# f2 <- estimateRelationship(y = yobs2, x = xobs, yunc = yunc, xunc = xunc, link = "linIntcp")
# 
# 
# ##############################################
# data <- data.frame(Category = c("Site1", "Site1", "Site1", "Site2", "Site2"),
#                    X1 = c(1, 0.9, 1.2, 4, 5),
#                    SD_X1 = c(0.2, 0.3, 0.2, 0.2, 0.3),
#                    X2 = c(1.5, 1.8, 1.1, 2.25, 2.3),
#                    SD_X2 = c(0.5, 0.3, 0.2, 0.2, 0.3))
# 
# yEstimates <- estimateY(relationship = "Y ~ 3 + 4.5 * f1(X1) * f2(X2) - f2(f1(X1))",
#                         regfunctions = list(f1 = f1, f2 = f2),
#                         indVars = c("X1", "X2"),
#                         data = data,
#                         indVarsUnc = c("SD_X1", "SD_X2"),
#                         category = "Category",
#                         n_samples = 10000,
#                         includeRegUnc = TRUE,
#                         distribution = "gamma")
# 
# plotDensities(yEstimates, type = "Category", plotType = "KernelDensity")
# results <- summariseEstimates(yEstimates, type = "Individual", probability = 0.95,
#                               checkDifferencesReference = TRUE,
#                               referenceType = "dist",
#                               referenceDist = "norm", referenceParameters = c(50, 3))
# 
# # with sample
# 
# results <- summariseEstimates(yEstimates, type = "Individual", probability = 0.95,
#                               checkDifferencesReference = TRUE,
#                               referenceType = "sample",
#                               referenceSample = c(60, 52, 75, 48, 50, 56))
# 
# # with reference Table
# referenceTable <- matrix(c(7,3,2,5,3,2,2,6), nrow = 2, byrow = TRUE)
# 
# results <- summariseEstimates(yEstimates, type = "Individual", probability = 0.95,
#                               checkDifferencesReference = TRUE,
#                               referenceType = "freqTable", referenceTable = referenceTable,
#                               referenceDist = "norm", referenceParameters = c(50, 3),
#                               referenceSample = c(60, 52, 75, 48, 50, 56))
