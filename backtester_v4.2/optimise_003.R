source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/Modify/limit1.R') 

numOfDays <- 1100
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier


lookbackLimit <- seq(from=26,to=80,by=2)

#lossLimits <- seq(from=0.1,to=2.1,by=1)
#profitTarget <- seq(from=0.1,to=2.1,by=1)

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
