mom.bt<-function(data,top,lookback){
  
  #Relative Momentum
  prices = data$prices
  n = ncol(prices)
  
  month.ends = endpoints(prices, 'months')  #monthly rebalance end points
  month.ends = month.ends[month.ends > 0] 
  
  ret = prices / mlag(prices) - 1  #daily return
  mom.r<-prices / mlag(prices,lookback) -1 #momentum factor
  
  weight = (prices) #create empty weight matrix
  weight[] = NA
  
  
  for(i in month.ends[month.ends>lookback]){
    
    asset.rank<-rank.mom(mom.r[i,]) #relative momentum rank
    asset.to.keep<-colnames(asset.rank[,which(asset.rank <= top)]) #top 2 assets
    
    weight[i,asset.to.keep]<-1/length(asset.to.keep)
    weight[i,which(is.na(weight[i,]) == T)]<-0
    
  }
  
  
  data$weight[] = NA
  data$weight[month.ends,] = weight[month.ends,]   
  capital = 100000
  data$weight[] = (capital / prices) * data$weight
  mom.relative = bt.run(data, type='share', capital=capital)
  return(mom.relative)
}


equal.weight.bt<-function(data){
  
  #equal weight
  prices = data$prices
  n = ncol(prices)
  
  month.ends = endpoints(prices, 'months')  #monthly rebalance end points
    month.ends = month.ends[month.ends > 0] 
  
  weight = (prices) #create empty weight matrix
    weight[] = NA
  
  #Equal Weight
  data$weight[] = NA
  data$weight[month.ends,] = ntop(prices[month.ends,], n)
  capital = 100000
  data$weight[] = (capital / prices) * data$weight
  equal.weight = bt.run(data, type='share')
  return(equal.weight)
}




