
#Install Packages#

#Load Required Libraries
library(quantmod)
library(TTR)
library(matrixStats)

#Variables to get instrument codes first.

EURCurrency.Pairs <- c("EUR/USD", "EUR/JPY", "EUR/NZD", "EUR/GBP","EUR/CHF","EUR/AUD", "EUR/CAD")

#create new environment to house all price data
newEnv = new.env()

#source data
fromDate = Sys.Date() - 180
toDate = Sys.Date()

getSymbols(EURCurrency.Pairs, src = "oanda",env=newEnv, from = fromDate, to = toDate)

#merge all price series
allTimeSeries = do.call(merge.xts,as.list(newEnv))

#convert column names to original form EUR.USD => EUR/USD
colnames(allTimeSeries) = gsub("[.]","\\/",colnames(allTimeSeries))

#reorder columns as per original order
allTimeSeries = allTimeSeries[,EURCurrency.Pairs]
allTimeSeriesFirst = allTimeSeries[1,]

#get currency value
EUR <- allTimeSeries
EURFirst <- EUR[1,]
#EUR$Value <- rowSums(allTimeSeries)
#sweep(EUR,2,EUR[1,], '/')
# value - 1


