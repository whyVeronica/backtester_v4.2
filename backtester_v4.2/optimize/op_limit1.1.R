source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/Modify/limit1.R') 

numOfDays <- 1100
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier


#for series1:
lookbackLimit <- seq(from=5,to=150,by=10)

#for series2:
#lookbackLimit <- seq(from=55,to=90,by=1)

#for series3: 1+
#lookbackLimit <- seq(from=5,to=25,by=1)

#for series4:
#not suitable?

#for series5:
#not suitable?

#for series6: 1+,2+
#lookbackLimit <- seq(from=40,to=150,by=1)

#for series7:
#lookbackLimit <- seq(from=30, to=120,by=1)

#for series8:
#lookbackLimit <- seq(from=5,to=95,by=10)

#for series9:
#lookbackLimit <- seq(from=5,to=95,by=10)

#for series10:
#lookbackLimit <- seq(from=5,to=95,by=10)



paramsList  <- list(lookbackLimit)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=2)
colnames(resultsMatrix) <- c("lookbackLimit","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (lbl in lookbackLimit) {
  params <- list(lookbackLimit=lbl,series=1,posSizes=rep(1,10)) 
  results <- backtest(dataList, getOrders, params, sMult)
  pfolioPnL <- plotResults(dataList,results)
  resultsMatrix[count,] <- c(lbl,pfolioPnL$fitAgg)
  pfolioPnLList[[count]]<- pfolioPnL
  cat("Just completed",count,"out of",numberComb,"\n")
  print(resultsMatrix[count,])
  count <- count + 1
}


print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])
