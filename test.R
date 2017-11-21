myNames <- letters[1:26]
myValues <- c(1:26)

dataTable = data.frame(myNames,myValues)
momCalculate <- function(dataTable,lookBackPeriod,sourceCol,targetCol)
{
  #dataTable[1,targetCol] <- dataTable[1,sourceCol]
  dataTable[1:nrow(dataTable),targetCol] <- dataTable[1:nrow(dataTable),sourceCol]+1
  
  n <- nrow(dataTable)
  
  for(i in 1:n)
  {
    if (i < lookBackPeriod)
    {
     # dataTable[i,targetCol] <- 0
    }
    if (i >= lookBackPeriod)
    {
      loop <- 0
      while (loop < lookBackPeriod)
      {
        if (i == (i-loop))
        {
          dataTable[i,targetCol] <- dataTable[i,targetCol]
        }
        else
        {
          dataTable[i,targetCol] <- dataTable[i,targetCol] + dataTable[(i-loop),targetCol]
        }
        loop <- loop + 1
      }
    }
    #dataTable[i,targetCol] <- (dataTable[i-1,targetCol] * dataTable[i,sourceCol])
    
    #if (i > lookBackPeriod)
    #{
    #  dataTable[i,targetCol] <- (dataTable[i,targetCol] / dataTable[i-lookBackPeriod,sourceCol])
    #}
  }
  
  return(dataTable)
}

dataTable = momCalculate(dataTable,3,"myValues","DiffPct")