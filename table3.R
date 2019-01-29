#In this part, we will compare the out of sample forecasting
#Performance of univariate regression-based forecast and principal
#Components based forecast with the benchmark historical average
#Forecast. This is Table3 in Neely(2014)

rm(list = ls())
#Replication means working with the exact same data as Neely
#If it is false, the the new and updated data will be given
#By the sourced getting_data.R
replication <- FALSE
output_to_csv <- FALSE

source("getting_data.R")

#First, pick the length of a build-up period for regressions 
#The out of sample predictions will start after this months
m <- 181 #length of build-up period
#Determining which columns contain fundamentals or 
#Technical indicators, which will be analyzed separately
macro_cols <- 3:16
tech_cols <- 17:30
var_cols <- c(macro_cols, tech_cols)
pcs <- c("PC_ECON", "PC_TECH", "PC_ALL")
n_add_fcst <- 4 #PC regressions plus 1 historical avg benchmark

#Needed number of rows for a dataframe storing results
v <- length(var_cols) + n_add_fcst

#Number of observations, 805 with new data, 733 with original
d17 <- dim(data_17)[1]
table3 <- data.frame(matrix(0, v, 1))
row.names(table3) <- c("HA", colnames(data_17)[var_cols], pcs)
colnames(table3) <- c("MSFE")

#----Creating Table 3 of the article------

# Historical average benchmark forecast
eqp <- data_17[,"EQP"]
ha_fcst <- cumsum(eqp) / seq_along(eqp)
#Mean squared forecast error
table3["HA", "MSFE"] <- 10000 * mean((eqp[((m + 1) : d17)] - ha_fcst[m : (d17 - 1)]) ^ 2)

#Getting the mean squared forecast error of a univariate regression forecast

FcErrorV <- function(var_name){
  x <- data_17[, var_name] #The predictor variable
  all_se = c() #This will contain the squared errors for each period
  for (i in m : (d17 - 1)){ #For every period starting from the end of the build-up
    #Univariate linear regression
    model <- lm(eqp[2 : i] ~ x[1 : (i - 1)])
    #Forecast of the regression
    r_pred <- sum(as.numeric(model$coefficients) * c(1, x[i]))
    #Error of the forecast for the period is se, we append it to all_se
    se <- (eqp[(i + 1)] - r_pred) ^ 2
    all_se <- c(all_se, se)
  }
  10000 * mean(all_se) #To have the same magnitude as in Neely(2014)
}

#Getting the mean squared forecast error of a PCA regression forecast

FcErrorP <- function(type){
  #The function works the same way as FcErrorV, after computing the needed PCA
  #Determining which PCA to compute
  if (type == "PC_ECON"){
    pca <- prcomp(data_17[, macro_cols], center = TRUE, scale = TRUE)
    n_comps <- 4
  }
  else if (type == "PC_TECH"){
    pca <- prcomp(data_17[, tech_cols], center = TRUE, scale = TRUE)
    n_comps <- 1
  }
  else{
    pca <- prcomp(data_17[, var_cols], center = TRUE, scale = TRUE)
    n_comps <- 5
  }
  x <- pca$x[, 1 : n_comps] #The predictor variables
  all_se = c()
  for (i in m : (d17 - 1)){
    if (n_comps == 1){
      model <- lm(eqp[2 : i] ~ x[1 : (i - 1)])
      r_pred <- sum(as.numeric(model$coefficients) * c(1, x[i]))
    }
    else{
      model <- lm(eqp[2 : i] ~ x[1 : (i - 1), ])
      r_pred <- sum(as.numeric(model$coefficients) * c(1, x[i, ]))
    }
    se <- (eqp[(i + 1)] - r_pred) ^ 2
    all_se <- c(all_se, se)
  }
  10000 * mean(all_se)
}  

#Computing and putting the results into the dataframe
for (i in colnames(data_17)[var_cols]){
  table3[i, "MSFE"] <- FcErrorV(i)
}

for (i in pcs){
  table3[i, "MSFE"] <- FcErrorP(i)
}

if (output_to_csv){
  folder <- "Tables"
  ifelse(!dir.exists(file.path(folder)), dir.create(file.path(folder)), FALSE)
  setwd(paste0(path, "/", folder))
  write.csv(table3, "table3.csv")
  setwd(path)
}