#In this part, 

rm(list = ls())
replication <- FALSE
#Replication means working with the exact same data as Neely
#If it is false, the the new and updated data will be given
#By the sourced getting_data.R
source("getting_data.R")

#Initializing or using an existing subfolder for the outputs
folder <- "Figures"
ifelse(!dir.exists(file.path(folder)), dir.create(file.path(folder)), FALSE)
setwd(paste0(path, "/", folder))

#Determining which columns contain fundamentals or 
#Technical indicators, which will be analyzed separately
macro_cols <- 3:16
tech_cols <- 17:30
var_cols <- c(macro_cols, tech_cols)

#-------Creating the plots of Figure 2 of the article-----------


#Only for fundamentals
pca_m <- prcomp(data_17[, macro_cols], center = TRUE, scale = TRUE)
#Only for technicals
pca_t <- prcomp(data_17[, tech_cols], center = TRUE, scale = TRUE)
#For both
pca_all <- prcomp(data_17[, var_cols], center = TRUE, scale = TRUE)

#This function plots the nth (pc_order) principal component of the
#Regression given by type, which is one of macro, tech, or is nothing
#Is given, the default is all. The function, if called also saves the plot
PCPlot <- function(pc_order, type){
  #Determining the PCA to compute
  if (type == "macro"){
    x <- data.frame(pca_m$rotation)
    title_s <- "Macroeconomic" #Title for the plot
  }
  else if (type == "tech"){
    x <- data.frame(pca_t$rotation)
    title_s <- "Technical"
  }
  else{
    x <- data.frame(pca_all$rotation)
    x["Vars"] <- row.names(x)
    title_s <- "All"
  }
  if (max(x[, pc_order]) != max(abs(x[, pc_order]))){
    x[, pc_order] <- -x[, pc_order]
  }
  x["Vars"] <- row.names(x)
  ggplot(x, aes(Vars, x[, pc_order])) +
    geom_bar(stat = "identity") +
    labs(title = paste(title_s, "PC", pc_order)) +
    xlab("") +
    ylab("") +
    scale_x_discrete(limits= x$Vars)
  ggsave(paste0(title_s, pc_order, ".jpeg"))
}

#Plot and save first 4 components of macro PCA. Note that the 
#Third principal component in Neely2014 became the fourth with new data
PCPlot(1, "macro")
PCPlot(2, "macro")
PCPlot(3, "macro")
PCPlot(4, "macro")

#Showing the importance change between the 3rd and 4th components
#The eigenvalues are very close indeed
plot(pca_m, main = "PC importances for Macroeconomic variables")

#Plot and save first component of tech PCA.
PCPlot(1, "tech")

#Plot and save first 5 components of PCA all. A similar change happened
#As with macro PCs with new data: the 3th component became the 5th
PCPlot(1, "all")
PCPlot(2, "all")
PCPlot(3, "all")
PCPlot(4, "all")
PCPlot(5, "all")

#---------Creating the plots of Figure 3-----------
#This figure shows the time series of principal components
#Throughout the examined period
PCEvolution <- function(pc_order, type){
  if (type == "macro"){
    pc <- pca_m
    x <- data.frame(pca_m$rotation)
    title_s <- "Macroeconomic"
  }
  else if (type == "tech"){
    pc <- pca_t
    x <- data.frame(pca_t$rotation)
    title_s <- "Technical"
  }
  else {
    pc <- pca_all
    x <- data.frame(pca_all$rotation)
    x["Vars"] <- row.names(x)
    title_s <- "All"
  }
  #neg_mult is used just for total comparability with the article
  #Where the loading with the biggest absolute value is always positive
  neg_mult <- 1
  if (max(x[, pc_order]) != max(abs(x[, pc_order]))){
    neg_mult <- -1
  }
  pc_ts <- data.frame("Date" = as.Date(data_17[,1]), "PC" = neg_mult * pc$x[, pc_order])
  ggplot(pc_ts, aes(Date, PC)) +
    geom_line() +
    labs(title = paste(title_s, "PC", pc_order, "evolution")) +
    xlab("") +
    ylab("") +
  ggsave(paste0(title_s, "PC", pc_order, "evol.jpeg")) 
  
}

PCEvolution(1, "macro")
PCEvolution(2, "macro")
PCEvolution(3, "macro")
PCEvolution(4, "macro")
PCEvolution(1, "tech")
PCEvolution(1, "all")
PCEvolution(2, "all")
PCEvolution(3, "all")
PCEvolution(4, "all")

setwd(path)
