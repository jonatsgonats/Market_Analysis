require(TTR)


faa.bt<-function(data,top,lookback,weight.mom,cash){
  #Absolute and Relative Momentum
  prices = data$prices
  n = ncol(prices)
  
  month.ends = endpoints(prices, 'months')  #monthly rebalance end points
  month.ends = month.ends[month.ends > 0] 
  
  ret = bt.apply.matrix(prices, ROC, type='continuous')
    mom.r<-ROC(prices,lookback,type='continuous')    #prices / mlag(prices,lookback) -1 #momentum factor
    mom.a<-mom.r*NA
    mom.a[month.ends,] = prices[month.ends,] / mlag(prices[month.ends,],4) - 1 #paper author implementation of momentum

  hv<- sqrt(252) * bt.apply.matrix(ret, runSD, n = 80) #voltility factor
  
  weight = (prices) #create empty weight matrix
  weight[] = NA
  
  for(i in month.ends[month.ends>lookback]){
    ##################################################
    #momentum rank
    mom.rank<-rank.mom(mom.a[i,])  
    
    #correlation rank
    hist<-ret[(i-lookback):i,]
    cor.hist<-cor(hist)
    diag(cor.hist)<-NA #take out the diagonal 
    avg.cor<-weight[i,] * NA
    
    for(j in 1:ncol(cor.hist)){
      avg.cor[,j]<-sum((cor.hist[,j]),na.rm=T)/(length(cor.hist[,j])-1) #avg correlation
    }
    cor.rank<-rank.mom(-avg.cor)
    
    #volatility rank
    vol.rank<-rank.mom(-hv[i,]) #volatility rank
    ##################################################
    
    meta.rank<-(weight.mom[1]*mom.rank) + (weight.mom[2]*vol.rank) + (weight.mom[3]*cor.rank)  #loss function for meta ranking
    meta.rank<-rank.mom(-meta.rank) #rank
    
    asset.to.keep<-colnames(meta.rank[,which(meta.rank <= top)]) #top n assets
    
    #Absolute momentum filter 
    current.assets<-mom.a[i,asset.to.keep] #momentum of Assets that passed the meta ranking
    temp<-which(current.assets<0)  #check for assets with negative momentum
    total.assets<-length(current.assets)
    
    if(!length(temp) == 0){  #if there exists an asset with negative momentum
    
      current.assets[,temp]<-NA
      asset.to.keep<-colnames(current.assets[,which(is.na(current.assets) == F)])
      
      if(cash %in% asset.to.keep){ #check if cash is already in the top assets
        asset.to.keep<-asset.to.keep[-which(asset.to.keep == cash)]
      }
    }
    
    weight[i,asset.to.keep]<-1/total.assets
    weight[i,which(is.na(weight[i,]) == T)]<-0
    if(!length(temp) == 0){
      weight[i,cash]<-1 - rowSums(weight[i,])
    }
  
  }
  
  data$weight[] = NA
  data$weight[month.ends,] = weight[month.ends,]   
  is.na(data$weight)<-0
  capital = 100000
  data$weight[] = (capital / prices) * data$weight
  faa = bt.run(data, type='share', capital=capital)
  
  return(faa)
}








