
abs.mom.bt<-function(data,lookback){
  #######################################################
  #Code Strategies 
  #######################################################
  
  #Absolute and Relative Momentum
  prices = data$prices
  n = ncol(prices)
  
  month.ends = endpoints(prices, 'months')  #monthly rebalance end points
  month.ends = month.ends[month.ends > 0] 
  
  ret = prices / mlag(prices) - 1  #daily return
  mom.r<-prices / mlag(prices,lookback) -1 #momentum factor
  
  weight = (prices) #create empty weight matrix
    weight[] = NA
  
  
  for(i in month.ends[month.ends>lookback]){
    abs.mom<-which(mom.r[i,] > 0)
    
    if(length(abs.mom) == 0){  #if there are no assets that pass the test
      weight[i,]<-0
    }else{
      weight[i,abs.mom]<-1/length(abs.mom)
      weight[i,which(is.na(weight[i,]) == T)]<-0
    }
  }
  
  
  data$weight[] = NA
  data$weight[month.ends,] = weight[month.ends,]   
  capital = 100000
  data$weight[] = (capital / prices) * data$weight
  abs.mom = bt.run(data, type='share', capital=capital)
  return(abs.mom)
}