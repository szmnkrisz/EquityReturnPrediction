#In this part, we compute univariate regressions and PCA-based regressions
#To see how capable the 14 fundamental and 14 technical variables are of 
#Forecasting equity risk premium. Besides p_values and R^2, a special R^2
#Is computed for both economic expansions and recessions, to see how 
#The performance of the variables in the 2 groups differ during economic cycles 
#This is Table2 in Neely(2014)

rm(list = ls())
#Replication means working with the exact same data as Neely
#If it is false, the the new and updated data will be given
#By the sourced getting_data.R
replication <- FALSE
output_to_csv <- FALSE

source("getting_data.R")

#-------Univariate regressions---------

#Determining which columns contain fundamentals or 
#Technical indicators, which will be analyzed separately
#With PCA
macro_cols <- 3:16
tech_cols <- 17:30
var_cols <- c(macro_cols, tech_cols)

#Names for the later-defined principal components, the
#First 4 for fundamentals, 1 for techs and 5 for the 
#PCA together
econ_comps <- c("ECON_F1", "ECON_F2", "ECON_F3", "ECON_F4")
tech_comps <- c("TECH_F1")
all_comps <- c("ALL_F1", "ALL_F2", "ALL_F3", "ALL_F4", "ALLF5")

v <- length(c(var_cols, econ_comps, tech_comps, all_comps))

#Number of observations, 805 with new data, 733 with original
d17 <- dim(data_17)[1]

#Storing results into data frame
table2 <- data.frame(matrix(0, v, 5))
row.names(table2) <- c(colnames(data_17)[var_cols], 
                       econ_comps, tech_comps, all_comps)
colnames(table2) <- c("Coefficient", "Rsq", "RsqExp", "RsqRec", "p_value")

#Equity risk premium
eqp = data_17[2 : d17,"EQP"]

#Writing the results into the df initialized above, for each of
#The 28 original variable, we run a univariate regression, and
#Get the slope, the p-value, the R^2, and the R^2 during expansion
#And R^2 during recession separately, according to Neely's formula
for (i in 1 : length(var_cols)){
  x <- data_17[1 : (d17 - 1), var_cols[i]]
  model <- lm(eqp ~ x)
  table2[i, "Coefficient"] <- model$coefficients[2]
  table2[i, "p_value"] <- summary(model)$coefficients[2 ,"Pr(>|t|)"]
  table2[i, "Rsq"] <- summary(model)$r.squared
  
  #Recession and Expansion R^2 values
  
  table2[i, "RsqRec"] <- 1 - sum(data_17[2 : d17, "REC"] * model$residuals ^ 2) /
    sum(data_17[2 : d17, "REC"] * (eqp - mean(eqp)) ^ 2)
  table2[i, "RsqExp"] <- 1 - sum((1 - data_17[2 : d17, "REC"]) * model$residuals ^ 2) /
    sum((1 - data_17[2 : d17, "REC"]) * (eqp - mean(eqp)) ^ 2)
}

#------PCA-----------
#Only for fundamentals
pca_m <- prcomp(data_17[, macro_cols], center = TRUE, scale = TRUE)
#Only for technicals
pca_t <- prcomp(data_17[, tech_cols], center = TRUE, scale = TRUE)
#For both
pca_all <- prcomp(data_17[, var_cols], center = TRUE, scale = TRUE)

#------PC-based regressions----------
#We run 3, possibly multivariate regressions for the first few principal
#Components of the fundamental, technical and all PCAs
#The stored results are the same as for the preceding univariate models
PCReg <- function(which_pca, comp_vector){
  x <- which_pca$x
  n_comps <- length(comp_vector)
  model <- lm(eqp ~ x[1 : (d17 - 1), 1 : n_comps])
  table2[comp_vector, "Coefficient"] <- model$coefficients[2 : (1 + length(comp_vector))]
  table2[comp_vector, "p_value"] <- summary(model)$coefficients[2 : (1 + length(comp_vector)), "Pr(>|t|)"]
  table2[comp_vector[1], "Rsq"] <- summary(model)$r.squared
  table2[comp_vector[1], "RsqRec"] <- 1 - sum(data_17[2 : d17, "REC"] * model$residuals ^ 2) /
    sum(data_17[2 : d17, "REC"] * (eqp - mean(eqp)) ^ 2)
  table2[comp_vector[1], "RsqExp"] <- 1 - sum((1 - data_17[2 : d17, "REC"]) * model$residuals ^ 2) /
    sum((1 - data_17[2 : d17, "REC"]) * (eqp - mean(eqp)) ^ 2)
  table2
}

table2 <- PCReg(pca_m, econ_comps)
table2 <- PCReg(pca_t, tech_comps)
table2 <- PCReg(pca_all, all_comps)

if (output_to_csv){
  folder <- "Tables"
  ifelse(!dir.exists(file.path(folder)), dir.create(file.path(folder)), FALSE)
  setwd(paste0(path, "/", folder))
  write.csv(table2, "table2.csv")
  setwd(path)
}