# -- Rquired Packages in order to use the R API
library("downloader")
library("RCurl")
library("jsonlite")
library("httr")

# -- Source ROanda API Usage
source("ROanda.R")

# -- character ---- # Your Account Type "practice" or "live"
OA_At <- "practice"  
# -- numeric ------ # Your Account ID 
OA_Ai <- 841086     
# -- character ---- # API Token Practice Account
OA_Ak <- "4647d4fd295460466a02faaa9e622356-0a6a91b37e8a49f2ef38729d05b61102"
# -- numeric ------ # Hour of the "End of the Day"
OA_Da <- 17
# -- character ---- # Time Zone in format "Continent/Zone 
OA_Ta <- "America/New_York"
# -- character ---- # Instrument in format "CURRENCY_CURRENCY"
OA_In <- "EUR_USD"
# -- character ---- # Granularity of the prices
OA_Gn <- "D"
# -- character ---- # Initial Date
OA_F1Px <- "2016-01-01"
# -- character ---- # Final Date
OA_F2Px <- "2016-08-07"
# -- character ---- # Order Type to place "market" or "limit"
OA_Ot <- "market"
# -- numeric ------ # Order's Lot Size to open
OA_Ls <- 1 
# -- character ---- # Side of the order, "buy" or "sell"
OA_Sd <- "buy"
# -- character ---- # Expery date of the order
OA_Ex <- "2016-09-01"
# -- numeric ------ # The price to execute the order
OA_Pr <- 1
# -- numeric ------ # Take Profit
OA_Tp <- 1
# -- numeric ------ # Stop Loss
OA_Sl <- 1
# -- numeric ------ # Trailing Stop
OA_Ts <- 1

################################
### Download historical data ###
################################
EURUSD_4HR <- HisPrices("practice","H4","17",OA_Ta,OA_Ak,OA_In,OA_F1Px,OA_F2Px,"2000")
#EURUSD_Daily <- HisPrices("practice","D","17",OA_Ta,OA_Ak,OA_In,OA_F1Px,OA_F2Px,"2000")

################################
### Get Difference           ### 
################################

#Explanation: Get difference of each row in column close compared to 
#previous row to get difference change in %
#example: c(NA,(EURUSD_4HR[2,5] - EURUSD_4HR[1,5]) / EURUSD_4HR[2,5] * 100)
EURUSD_4HR$DiffPct <- 
  c(NA,(EURUSD_4HR[2:nrow(EURUSD_4HR),5] - EURUSD_4HR[1:(nrow(EURUSD_4HR)-1),5]) 
    / EURUSD_4HR[2:nrow(EURUSD_4HR),5] * 100)

#Round decimal of DiffPct to 4 places
EURUSD_4HR$DiffPct <- round(EURUSD_4HR$DiffPct,4)


################################
### Generic Momentum         ### 
################################
#Formula: (multiply each row from 1+Return) - 1
#Look Back Period
LookBackDAY <- 12
LookBack4HR <- 12

#EURUSD_4HR$MOM <- 
#  c(NA,)

for(i in 1:nrow(EURUSD_4HR))
{
  EURUSD_4HR$Mom[i,] <- c(NA,EURUSD_4HR[i,8])
}
