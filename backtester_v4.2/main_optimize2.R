source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/W%R-MACD(pos-P&R)4.R') 

numOfDays <- 550
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

thresholdSeq <- seq(from=20,to=50,by=5)
#sdParamSeq  <- seq(from=1.5,to=2,by=0.5) 
paramsList  <- list(thresholdSeq)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=2)
colnames(resultsMatrix) <- c("threshold","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (th in thresholdSeq) {
  
    params <- list(threshold=th,lookbackR=14,
                   nFast=12, nSlow=26, nSig=9,series=1:10,posSizes=rep(1,10)) 
    results <- backtest(dataList, getOrders, params, sMult)
    pfolioPnL <- plotResults(dataList,results)
    resultsMatrix[count,] <- c(th,pfolioPnL$fitAgg)
    pfolioPnLList[[count]]<- pfolioPnL
    cat("Just completed",count,"out of",numberComb,"\n")
    print(resultsMatrix[count,])
    count <- count + 1
  
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])
