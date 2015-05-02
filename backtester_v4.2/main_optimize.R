source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/W%R(pos-P&R)3.R') 

numOfDays <- 550
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

#lookbackR <- 14
#threshold <- 30
#threshold <- seq(from=20,to=50, by=5)
inventoryLimit <- seq(from=50,to=400,by=50)

paramsList  <- list(inventoryLimit)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=2)
colnames(resultsMatrix) <- c("inventoryLimit","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (inv in inventoryLimit) {
    #for(th in threshold) {
      params <- list(lookbackR=14,lookbackS=5,lookbackL=30,threshold=30,
                     inventoryLimit=inv,series=1:10,posSizes=rep(1,10)) 
      results <- backtest(dataList, getOrders, params, sMult)
      pfolioPnL <- plotResults(dataList,results)
      resultsMatrix[count,] <- c(inv,pfolioPnL$fitAgg)
      pfolioPnLList[[count]]<- pfolioPnL
      cat("Just completed",count,"out of",numberComb,"\n")
      print(resultsMatrix[count,])
      count <- count + 1
    #}    
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])
