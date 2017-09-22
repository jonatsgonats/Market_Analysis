timing.strategy.local <- function
(
  data,
  periodicity = 'months',
  ma.len = 200

) 
{
  #*****************************************************************
  # Load historical data 
  #****************************************************************** 
  
  prices = data$prices   
  n = ncol(prices)
  nperiods = nrow(prices)
  
  # find period ends
  period.ends = endpoints(data$prices, periodicity)
  period.ends = period.ends[period.ends > 0]
  
  
  #*****************************************************************
  # Code Strategies
  #****************************************************************** 	
  position.score = prices
    
  # BuyRule, price > 10 month SMA
  sma = bt.apply.matrix(prices, SMA, ma.len)
  buy.rule = prices > sma
  buy.rule = ifna(buy.rule, F)
  
  weight = ntop(position.score[period.ends,], n)
    weight<-weight*buy.rule[period.ends,]
  
  data$weight[] = NA
  data$weight[period.ends,]  = weight
  timing = bt.run.share(data, clean.signal=F, trade.summary=T)
  
  return(timing)
}
