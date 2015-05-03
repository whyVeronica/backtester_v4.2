source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/latest/W%R-MACD-MA3-Crossover-stoploss-targetProfit1.R') 

numOfDays <- 1100
dataList <- getData(directory="PART3")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier


lookbackR <- seq(from=10, to=40, by=5)
lookbackS <- seq(from=5, to=11,by=3)
lookbackL <- seq(from=10, to=50, by=5)
threshold <- seq(from=20,to=49,by=10)
lossLimits <- seq(from=0.1,to=20.1,by=5)
profitTarget <- seq(from=0.1,to=20.1,by=5)

paramsList  <- list(lookbackS,lookbackL,lookbackR,threshold,lossLimits,profitTarget)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=7)
colnames(resultsMatrix) <- c("lookbackS","lookbackL","lookbackR",
                             "threshold","lossLimits","profitTarget","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (lbs in lookbackS) {
  for (lbl in lookbackL) {
    if(lbs < lbl){
      for (lbr in lookbackR) {
        for (th in threshold) {
            for (ll in lossLimits){
              for (pt in profitTarget){
                params <- list(lookbackS=lbs,lookbackL=lbl,lookbackR=lbr,
                               threshold=th,lossLimits=ll, profitTarget=pt,
                               series=9,posSizes=rep(1,10)) 
                results <- backtest(dataList, getOrders, params, sMult)
                pfolioPnL <- plotResults(dataList,results)
                resultsMatrix[count,] <- c(lbs,lbl,lbr,th,ll,pt,pfolioPnL$fitAgg)
                pfolioPnLList[[count]]<- pfolioPnL
                cat("Just completed",count,"out of",numberComb,"\n")
                print(resultsMatrix[count,])
                count <- count + 1
              }
            }
        }
      }
    }
    
  }
}


print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])
