source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/Latest/W%R-MACD-MA5-Crossover-profitPos-limit1.R') 

numOfDays <- 1100
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

#params <- list(lookbackR=14,lookbackS=5,lookbackL=8,threshold=40,nWait=0,
#nFast=5, nSlow=10, nSig=5, 
#lossLimits=8000, profitTarget=10000, series=c(1:10))#W%R & macd 

lookbackWRL <- seq(from=10, to=40, by=5)
lookbackS <- seq(from=5, to=11,by=3)
lookbackL <- seq(from=10, to=50, by=5)
threshold <- seq(from=25,to=49,by=5)
#threshold<-30
#nFast <- seq(from=5,to=15,by=10)
#nFast<-12
#nSlow <- seq(from=10,to=50,by=20)
#nSlow<-26
#nSig <- seq(from=5,to=20,by=10)
#nSig<-9
#nWait<-seq(from=0,to=3,by=2)
#lossLimits <- seq(from=1000,to=5000,by=2000)
#profitTarget <- seq(from=1000,to=5000,by=2000)
#lookbackLimit <- seq(from=10,to=30,by=10)
#lookbackLimit <- 14

paramsList  <- list(lookbackS,lookbackL,lookbackWRL,threshold)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=5)
colnames(resultsMatrix) <- c("lookbackS","lookbackL","lookbackWRL",
                             "threshold","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (lbs in lookbackS) {
  for (lbl in lookbackL) {
    if(lbs < lbl){
      for (lbr in lookbackWRL) {
        for (th in threshold) {
          #for (nf in nFast) {
          #for (ns in nSlow) {
          #if(ns>nf){
          #for (nsig in nSig){
          #for(lbli in lookbackLimit){
          #for (ll in lossLimits){
            #for (pt in profitTarget){
              params <- list(lookbackS=lbs,lookbackL=lbl,lookbackWRL=lbr,
                             threshold=th,series=1,posSizes=rep(1,10)) 
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
