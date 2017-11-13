
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

#Initial Value Face 2004 June 01
InitialEURUSD = 1.22460
InitialEURGBP = 0.6656
InitialEURAUD = 1.7303
InitialEURCAD = 1.6757
InitialEURNZD = 1.9478
InitialEURJPY = 135.46
InitialEURCHF = 1.5270
InitialEUR = c(InitialEURUSD,InitialEURJPY,InitialEURNZD,InitialEURGBP,InitialEURCHF,InitialEURAUD,InitialEURCAD)
#colnames(InitialEUR) = c("EUR/USD","EUR/JPY","EUR/NZD","EUR/GBP","EUR/CHF","EUR/AUD","EUR/CAD")

#merge all price series
allTimeSeries = do.call(merge.xts,as.list(newEnv))

#convert column names to original form EUR.USD => EUR/USD
colnames(allTimeSeries) = gsub("[.]","\\/",colnames(allTimeSeries))

#reorder columns as per original order
allTimeSeries = allTimeSeries[,EURCurrency.Pairs]

#get currency value
EUR <- allTimeSeries
EUR$Index <- 100 + (rowMeans((sweep(EUR,2,InitialEUR, '/')-1)*100))


#historical values
#EURDayHistory = fread("./historical/EURAUD1440.csv",select=c(1))
#colnames(EURDayHistory) <- c("Date")
#EURDayHistory$EURUSD = fread("./historical/EURUSD1440.csv",select=c(6))
#EURDayHistory$EURJPY = fread("./historical/EURJPY1440.csv",select=c(6))
#EURDayHistory$EURNZD = fread("./historical/EURNZD1440.csv",select=c(6))
#EURDayHistory$EURGBP = fread("./historical/EURGBP1440.csv",select=c(6))
#EURDayHistory$EURCHF = fread("./historical/EURCHF1440.csv",select=c(6))
#EURDayHistory$EURAUD = fread("./historical/EURAUD1440.csv",select=c(6))
#EURDayHistory$EURCAD = fread("./historical/EURCAD1440.csv",select=c(6))
#write.csv(EURDayHistory,"EURDayHistory.csv")
