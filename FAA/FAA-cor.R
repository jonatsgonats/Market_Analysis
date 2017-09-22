
cor.bt<-function(data,top,lookback){
  #######################################################
  #Code Strategies 
  #######################################################
  
  #cor backtest
  prices = data$prices
  n = ncol(prices)
  
  month.ends = endpoints(prices, 'months')  #monthly rebalance end points
  month.ends = month.ends[month.ends > 0] 
  
  ret = prices / mlag(prices) - 1  #daily return
  
  weight = (prices) #create empty weight matrix
  weight[] = NA
    
  for(i in month.ends[month.ends>lookback]){
    hist<-ret[(i-lookback):i,]
    cor.hist<-cor(hist)
    diag(cor.hist)<-NA #take out the diagonal 
    
    avg.cor<-weight[i,] * NA#matrix(NA,nrow=1,ncol=n)
    
    for(j in 1:ncol(cor.hist)){
      avg.cor[,j]<-sum((cor.hist[,j]),na.rm=T)/(length(cor.hist[,j])-1) #avg correlation
    }
    
    
    asset.rank<-rank.mom(-avg.cor) #correlation rank
    asset.to.keep<-colnames(asset.rank[,which(asset.rank <= top)]) #top 2 assets
    weight[i,asset.to.keep]<-1/top
    weight[i,which(is.na(weight[i,]) == T)]<-0
  }
  
  data$weight[] = NA
  data$weight[month.ends,] = weight[month.ends,]   
  capital = 100000
  data$weight[] = (capital / prices) * data$weight
  cor = bt.run(data, type='share', capital=capital)
  return(cor)
}



