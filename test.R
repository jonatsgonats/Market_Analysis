myNames <- letters[1:26]
myValues <- c(1:26)

dataTable = data.frame(myNames,myValues)
momCalculate <- function(dataTable,lookBackPeriod,sourceCol,targetCol)
{
  n <- nrow(dataTable)
  
  for(i in 1:n)
  {
    if (i < lookBackPeriod)
    {
      dataTable[i,targetCol] <- 0
    }
    if (i >= lookBackPeriod)
    {
    loop <- 0
      while (loop < lookBackPeriod)
      {
        if (i == (i-loop))
        {
          dataTable[i,targetCol] <- dataTable[i,sourceCol]+1
        }
        else
        {
          dataTable[i,targetCol] <- (dataTable[i,targetCol]) + (dataTable[(i-loop),sourceCol]+1)
        }
        loop <- loop + 1
      }
    }
  }
  return(dataTable)
}

dataTable = momCalculate(dataTable,3,"myValues","DiffPct")