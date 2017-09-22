
vol.bt<-function(data,top,lookback){
  #######################################################
  #Code Strategies 
  #######################################################
  
  #Volatility backtest
  prices = data$prices
  n = ncol(prices)
  
  month.ends = endpoints(prices, 'months')  #monthly rebalance end points
  month.ends = month.ends[month.ends > 0] 
  
  ret = prices / mlag(prices) - 1  #daily return
  
  weight = (prices) #create empty weight matrix
  weight[] = NA
  
  hv<- sqrt(252) * bt.apply.matrix(ret, runSD, n = lookback) #voltility factor
  
  
  for(i in month.ends[month.ends>lookback]){
    
    asset.rank<-rank.mom(-hv[i,]) #volatility rank
    asset.to.keep<-colnames(asset.rank[,which(asset.rank <= top)]) #top 2 assets
    weight[i,asset.to.keep]<-1/top
    weight[i,which(is.na(weight[i,]) == T)]<-0
  }
  
  data$weight[] = NA
  data$weight[month.ends,] = weight[month.ends,]   

  capital = 100000
  data$weight[] = (capital / prices) * data$weight
  vol = bt.run(data, type='share', capital=capital)
  return(vol)
}