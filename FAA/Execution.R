############################################################
#Flexible Asset Allocation (Keller & Putten, 2012)
#
#Replication by Systematic Edge
############################################################
rm(list=ls())
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)
load.packages("TTR,PerformanceAnalytics,quantmod,lattice")

#######################################################
#Get and Prep Data
#######################################################
setwd("C:/Users/michaelguan326/Dropbox/Code Space/R/blog research/FAA")

data <- new.env()
#tickers<-spl("VTI,IEF,TLT,DBC,VNQ,GLD")

tickers<-spl("VTSMX,FDIVX,VEIEX,VFISX,VBMFX,QRAAX,VGSIX")
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)

bt.prep(data, align='remove.na', dates='1990::2013')

#Helper
#Rank Helper Function
rank.mom<-function(x){
  if(ncol(x) == 1){
    r<-x
    r[1,1] <- 1
  }else{
    r <- as.xts(t(apply(-x, 1, rank, na.last = "keep")))
  }
  
  return(r)
}


#######################################################
#Run Strategies
#######################################################

source("C:/Users/michaelguan326/Dropbox/Code Space/R/blog research/FAA/FAA-mom.R")
source("C:/Users/michaelguan326/Dropbox/Code Space/R/blog research/FAA/FAA-abs-mom.R")
source("C:/Users/michaelguan326/Dropbox/Code Space/R/blog research/FAA/FAA-vol.R")
source("C:/Users/michaelguan326/Dropbox/Code Space/R/blog research/FAA/FAA-cor.R")
source("C:/Users/michaelguan326/Dropbox/Code Space/R/blog research/FAA/FAA.R")
source("C:/Users/michaelguan326/Dropbox/Code Space/R/blog research/FAA/FAA-bench.R")
models<-list()
top<-3
lookback<-80

#run models
models$mom<-mom.bt(data,top,lookback)  #relative momentum factor
models$abs.mom<-abs.mom.bt(data,lookback) #absolute momentum factor
models$vol<-vol.bt(data,top,lookback)  #volatility momentum factor
models$cor<-cor.bt(data,top,lookback)  #volatility factor
models$faber<-timing.strategy.local(data,'months',ma.len=200) #faber
models$ew<-equal.weight.bt(data)  #equal weight benchmark

#FAA model
models$faa<-faa.bt(data,top,lookback,
                   weight.mom=c(1,0.5,0.5),cash = "VFISX")


#report
plotbt.custom.report.part1(models)
plotbt.custom.report.part2(models)

plotbt.strategy.sidebyside(models)



