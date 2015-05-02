source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/W%R(pos-P&R).R') 

numOfDays <- 550
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

thresholdSeq <- seq(from=20,to=50,by=10)
inventoryLimitSeq  <- seq(from=100,to=400,by=100) 
paramsList  <- list(thresholdSeq,inventoryLimitSeq)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=3)
colnames(resultsMatrix) <- c("threshold","inventoryLimit","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (th in thresholdSeq) {
  for (In in inventoryLimitSeq){
    params <- list(threshold=th,lookbackR=14,
                   inventoryLimit=In,series=1:10,posSizes=rep(1,10)) 
    results <- backtest(dataList, getOrders, params, sMult)
    pfolioPnL <- plotResults(dataList,results)
    resultsMatrix[count,] <- c(th,In,pfolioPnL$fitAgg)
    pfolioPnLList[[count]]<- pfolioPnL
    cat("Just completed",count,"out of",numberComb,"\n")
    print(resultsMatrix[count,])
    count <- count + 1
  }
  
  
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])
