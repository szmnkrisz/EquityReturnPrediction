#This part of the code gets the necessary packages and
#establishes the dataframe according to whether one wants to
#use the original data until 2011, or the updated until 2017

path <- "~/EmpiricalFinanceResearch/EquityReturnPrediction"

library(openxlsx)
library(TTR)
library(ggplot2)

if (replication){
  source("data_original.R")
} else{
  source("data_updated.R")
}