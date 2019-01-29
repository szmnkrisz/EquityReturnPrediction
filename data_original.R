#This script is not for use on its own. It is called by getting_data.R
#This part of the code creates the original dataframe used by Neely(2014)
input_file <- "/Inputs/Returns_econ_tech_results.xlsx"

#The data is from December 1950 to December 2011
dates <-seq.Date(as.Date("1950-12-01"), as.Date("2011-12-01"), by = "month")

#orig_data will contains the values in the period for equity risk premium,
#14 fundamental variables and 14 technical indicators
orig_eq <- read.xlsx(paste0(path, input_file), sheet = 1)[288:1020,]
orig_macro <- read.xlsx(paste0(path, input_file), sheet = 2)[288:1020,]
orig_tech <- read.xlsx(paste0(path, input_file), sheet = 3)[288:1020,]
orig_data <- cbind(dates, orig_eq[,2], orig_macro[, 2:17], orig_tech[, 2:15])

#For part of the analysis, recession dummies are needed, which show whether
#the month was a month of economic expansion (0) or recession (1)
rec_dum <- read.xlsx(paste0(path, input_file), sheet = 1)
rec_dum <- rec_dum[which(rec_dum["Date"] == "195012") : which(rec_dum["Date"] == "201112"), "Recession.dummies"]

#Renaming and extracting the needed variables
colnames(orig_data) <- c("dates", "EQP", "DP", "DY", "EP", "DE", "RVOL", "BM", "NTIS", "TBL", "LTY", 
           "LTR", "TMS", "DFY",  "DFR", "INFL", "Rfree", "E12", "MA1",  "MA2",  "MA3",  "MA4",
           "MA5", "MA6", "MOM1", "MOM2", "OBV1", "OBV2", "OBV3", "OBV4", "OBV5", "OBV6")
for (c in colnames(orig_data)) if (c != "dates") orig_data[,c] <- as.numeric(orig_data[,c])
orig_data[c("Rfree", "E12")] <- NULL

#The dataframe other scripts can handle must have the name data_17
data_17 <- orig_data
rm(orig_data)

# Getting the recession dummies, after 2011 it is always 0
data_17["REC"] <- c(rec_dum, rep(0, dim(data_17)[1] - length(rec_dum)))