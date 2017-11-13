
#Install Packages#

#Load Required Libraries
library(quantmod)
library(TTR)
library(matrixStats)
library(data.table)
library(zoo)

#Variables to get instrument codes first.
EURCurrency.Pairs <- c("EUR/USD", "EUR/JPY", "EUR/NZD", "EUR/GBP","EUR/CHF","EUR/AUD", "EUR/CAD")

#create new environment to house all price data
newEnv = new.env()
FixedEnv = new.env()

#source data
fromDate = Sys.Date() - 180
toDate = Sys.Date()

getSymbols(EURCurrency.Pairs, src = "oanda",env=newEnv, from = fromDate, to = toDate)

#merge all price series
allTimeSeries = do.call(merge.xts,as.list(newEnv))

#convert column names to original form EUR.USD => EUR/USD
colnames(allTimeSeries) = gsub("[.]",".",colnames(allTimeSeries))

#reorder columns as per original order
#allTimeSeries = allTimeSeries[,EURCurrency.Pairs]

EUR2 <- allTimeSeries

#EUR2$Index <- 100 + (rowMeans((sweep(EUR,2,InitialEUR, '/')-1)*100))

#df$diff <- c(NA,df[2:nrow(df), 2] - df[1:(nrow(df)-1), 1])

EUR2$diff <- c(NA,EUR2[2:nrow(EUR2), 2] - EUR[1:(nrow(EUR2)-1),1])

