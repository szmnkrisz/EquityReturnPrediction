#This script is not for use on its own. It is called by getting_data.R
#This part establishes the new and updated dataframe, with data from
#Dec 1950 to Dec 2017. We have 6 years more data, and the previous data
#was also updated, so the results of the two analyses are different

input_file <- "/Inputs/PredictorData2017.xlsx"
input_file2 <- "/Inputs/spx.csv"
input_file3 <- "/Inputs/Returns_econ_tech_results.xlsx"

#Data from aMit Goyal's webpage, containing monthly equity risk premium,
#values for fundamental and technical variables
raw_data <- read.xlsx(paste0(path, input_file))

#Data of the SNP500 stock index to calculate technical rules
spx <- read.csv(paste0(path, input_file2))[, c(1, 6, 7)] #for volume-based strategies

#Recession dummies indicating whether the period was economic
#expansion (0) or recession (1)
rec_dum <- read.xlsx(paste0(path, input_file3), sheet = 1)
rec_dum <- rec_dum[which(rec_dum["Date"] == "195012") : which(rec_dum["Date"] == "201112"), "Recession.dummies"]

#Initializing the dataframe, data_17 (data until 2017 Dec)

from_t <- as.Date("1950-01-01")
to_t <- as.Date("2017-12-01")
date <- seq.Date(from <- from_t, to_t, by = "month")
data_17 <- data.frame(Date = date)

#-------Determining The Risk Premium----------

s <- which(raw_data[,1] == "195001") #row of starting period
e <- which(raw_data[,1] == "201712") #row of end period

#Value-weighted equity returns incl. dividends, in accordance to Neely
EQ <- raw_data[s :  e,
               c("yyyymm", "CRSP_SPvw")]
EQ[, 2] <- as.numeric(EQ[,2])

#Lag of risk-free rate
RF <- raw_data[(s - 1) :  (e - 1),
               c("yyyymm", "Rfree")]

data_17["EQP"] <- log(1 + EQ[, 2]) - log(1 + RF[, 2])

#--------14 Fundamental Variables

data_17["DP"] <- log(raw_data[s : e,"D12"] / raw_data[s : e,"Index"]) #Dividend-Price Ratio
data_17["DY"] <- log(raw_data[s : e,"D12"] / raw_data[(s - 1) : (e - 1), "Index"]) #Dividend Yield
data_17["EP"] <- log(raw_data[s : e,"E12"] / raw_data[s : e,"Index"]) #Earning-Price Ratio
data_17["DE"] <- log(raw_data[s : e,"D12"] / raw_data[s : e,"E12"]) #Dividend-Payout Ratio

#Equity risk premium volatility (Mele 2007, JFE)

for (i in 12 : dim(data_17)[1]){
  data_17[i, "RVOL"] <- sqrt(pi / 2) * sqrt(12) * (1 / 12) *
    sum(abs(as.numeric(data_17[(i - 11) : i, "EQP"])))
}

data_17["BM"] <- as.numeric(raw_data[s : e, "b/m"]) #Book-To-Market
data_17["NTIS"] <- - as.numeric(raw_data[s : e, "ntis"]) #Net Equity Expansion
data_17["TBL"] <- - 100 * as.numeric(raw_data[s : e, "tbl"]) #Treasury Bill Rate
data_17["LTY"] <- - 100 * as.numeric(raw_data[s : e, "lty"]) #Long-Term Yield
data_17["LTR"] <- - 100 * as.numeric(raw_data[s : e,"ltr"]) #Long-Term Return (Government Bonds)
data_17["TMS"] <- - 100 * (data_17["LTY"] - data_17["TBL"]) #Term Spread
data_17["DFY"] <- - 100 * (as.numeric(raw_data[s : e,"BAA"]) - as.numeric(raw_data[s : e,"AAA"])) #Default Yield Spread
data_17["DFR"] <- - 100 * (as.numeric(raw_data[s : e,"corpr"]) - as.numeric(raw_data[s : e,"ltr"])) #Default Return Spread
data_17["INFL"] <- - 100 * as.numeric(raw_data[(e - 1) : (s - 1),"infl"]) #Inflation (t-1 used to account for decay in CPI releases)

#-------14 Technical Indicators-------

#6 Moving Average rules

months_short <- c(1, 2, 3)
months_long <- c(9, 12)
index <- ts(raw_data[s : e,c("yyyymm", "Index")]) #date and the price level
moving_avgs <- data.frame("date" = index[, 1])

#Determining moving averages of the price
for (i in c(months_short, months_long)){
  moving_avgs[, paste0("ma_",i)] = SMA(index[, 2], i)
}

for (i in months_short){
  for (j in months_long){
    # 1, if the Short MA >= Long MA, 0 otherwise
    data_17[paste0("ma_",i,"_",j)] <- as.numeric(
      moving_avgs[, paste0("ma_", i)] >= moving_avgs[,  paste0("ma_", j)])
  }
}

#2 MOMENTUM rules

index_ts <- ts(index[, 2])
mom_months <- c(9, 12)
d17 <- dim(data_17)[1]

#Determining MOM signals, 1 if the current price is greater than the
#price i months ago, 0 otherwise
for (i in mom_months){
  data_17[paste0("MOM_",i)] <- c(rep(NA, i), as.numeric(index_ts >= lag(index_ts, -i)))[1 : d17]
}
data_17[12, "MOM_12"] <- 1 #needed for PCA, and also true

#6 On Balance Volume rules (methdology as in Neely 2014)

index_ts <- ts(spx[, "Adj.Close"]) 
vol_ts <- ts(spx[, "Volume"])

#vol_d is the monthly volume multiplied by 1 if the price just went up,
#by -1 if it just went down
vol_d <- c(0, (2 * (index_ts >= lag(index_ts, -1)) - 1) * vol_ts)
obv <- cumsum(vol_d) #on-basis volume for every period

#The signal is 1, if the short moving average of the OBV is greater
#than the long moving average, 0 otherwise

for (i in months_short){
  for (j in months_long){
    # 1, if the short obv moving average is not less than the long obv moving average
    data_17[paste0("obv_",i,"_",j)] <- as.numeric(SMA(obv, i) >= SMA(obv, j))[1 : d17]
  }
}

data_17 <- data_17[max(mom_months) : d17,]

#Adding in the recession dummies, after 2011 it is always 0
data_17["REC"] <- c(rec_dum, rep(0, dim(data_17)[1] - length(rec_dum)))