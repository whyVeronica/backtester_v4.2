source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/W%R-MACD(pos-P&R)5.R') 

numOfDays <- 550
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
#dataList <- lapply(dataList, function(x) x[(numOfDays+1):1100])
sMult <- 0.2 # slippage multiplier

thresholdSeq <- seq(from=20,to=50,by=10)
nWaitSeq <- seq(from=0,to=3,by=1)
#inventoryLimitSeq  <- seq(from=100,to=400,by=100) 
paramsList  <- list(thresholdSeq,nWaitSeq)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=3)
colnames(resultsMatrix) <- c("Threshold","waiting Days","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (th in thresholdSeq) {
  for (wa in nWaitSeq){
    params <- list(threshold=th,lookbackR=14,nWait=wa,
                   nFast=12, nSlow=26, nSig=9,series=1:10,posSizes=rep(1,10)) 
    results <- backtest(dataList, getOrders, params, sMult)
    pfolioPnL <- plotResults(dataList,results)
    resultsMatrix[count,] <- c(th,wa,pfolioPnL$fitAgg)
    pfolioPnLList[[count]]<- pfolioPnL
    cat("Just completed",count,"out of",numberComb,"\n")
    print(resultsMatrix[count,])
    count <- count + 1
  }
  
  
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])
