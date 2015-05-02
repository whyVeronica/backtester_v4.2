source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/latest/W%R-MACD-MA3-Crossover-stoploss-targetProfit1.R') 

numOfDays <- 1100
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

#params <- list(lookbackR=14,lookbackS=5,lookbackL=8,threshold=40,nWait=0,
#nFast=5, nSlow=10, nSig=5, 
#lossLimits=8000, profitTarget=10000, series=c(1:10))#W%R & macd 

lookbackR <- seq(from=10, to=40, by=5)
lookbackS <- seq(from=5, to=11,by=3)
lookbackL <- seq(from=10, to=50, by=5)
threshold <- seq(from=20,to=49,by=10)
#threshold<-30
#nFast <- seq(from=5,to=15,by=10)
#nFast<-12
#nSlow <- seq(from=10,to=50,by=20)
#nSlow<-26
#nSig <- seq(from=5,to=20,by=10)
#nSig<-9
#nWait<-seq(from=0,to=3,by=2)
lossLimits <- seq(from=0.1,to=2,by=0.5)
profitTarget <- seq(from=0.1,to=2,by=0.5)
#lookbackLimit <- seq(from=10,to=30,by=10)
#lookbackLimit <- 14

paramsList  <- list(lookbackS,lookbackL,lookbackR,threshold)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=5)
colnames(resultsMatrix) <- c("lookbackS","lookbackL","lookbackR",
                             "threshold","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (lbs in lookbackS) {
  for (lbl in lookbackL) {
    if(lbs < lbl){
      for (lbr in lookbackR) {
        for (th in threshold) {
          #for (nf in nFast) {
          #for (ns in nSlow) {
          #if(ns>nf){
          #for (nsig in nSig){
          #for(lbli in lookbackLimit){
          #for (ll in lossLimits){
          #for (pt in profitTarget){
          params <- list(lookbackS=lbs,lookbackL=lbl,lookbackR=lbr,
                         threshold=th,series=2,posSizes=rep(1,10)) 
          results <- backtest(dataList, getOrders, params, sMult)
          pfolioPnL <- plotResults(dataList,results)
          resultsMatrix[count,] <- c(lbs,lbl,lbr,th,pfolioPnL$fitAgg)
          pfolioPnLList[[count]]<- pfolioPnL
          cat("Just completed",count,"out of",numberComb,"\n")
          print(resultsMatrix[count,])
          count <- count + 1
          #}
          #}
          #}
          
          #}
          #}
          #}
          #}
        }
      }
    }
    
  }
}


print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])
