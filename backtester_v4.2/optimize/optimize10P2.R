source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/wait2.R') 

numOfDays <- 1100
dataList <- getData(directory="PART2")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

lookbackR <- seq(from=10, to=40, by=5)
lookbackS <- seq(from=5, to=11,by=3)
lookbackL <- seq(from=10, to=50, by=5)
threshold <- seq(from=20,to=49,by=5)
nWait <- seq(from=0,to=2,by=1)
#lossLimits <- seq(from=0.1,to=2.1,by=1)
#lossLimits <- seq(from=50,to=500,by=100)
#profitTarget <- seq(from=5,to=10,by=2)

paramsList  <- list(lookbackS,lookbackL,lookbackR,threshold,nWait)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=6)
colnames(resultsMatrix) <- c("lookbackS","lookbackL","lookbackR",
                             "threshold","nWait","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (lbs in lookbackS) {
  for (lbl in lookbackL) {
    if(lbs < lbl){
      for (lbr in lookbackR) {
        for (th in threshold) {
          for (nw in nWait){
            params <- list(lookbackS=lbs,lookbackL=lbl,lookbackR=lbr,
                           threshold=th,  
                           nWait=nw,series=10,posSizes=rep(1,10)) 
            results <- backtest(dataList, getOrders, params, sMult)
            pfolioPnL <- plotResults(dataList,results)
            resultsMatrix[count,] <- c(lbs,lbl,lbr,th,nw,pfolioPnL$fitAgg)
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


print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])
