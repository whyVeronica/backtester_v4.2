source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/W%R-MACD-MA6-Crossover-Original1.R') 

numOfDays <- 1100
dataList <- getData(directory="PART2")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

#params <- list(lookbackR=14,lookbackS=5,lookbackL=8,threshold=40,nWait=0,
#nFast=5, nSlow=10, nSig=5, 
#lossLimits=8000, profitTarget=10000, series=c(1:10))#W%R & macd 

lookbackR <- seq(from=10, to=30, by=2)
lookbackS <- seq(from=1, to=10,by=2)
lookbackL <- seq(from=8, to=36, by=4)
threshold <- seq(from=10,to=50,by=5)
#threshold<-30
nFast <- seq(from=3,to=15,by=3)
#nFast<-12
nSlow <- seq(from=8,to=26,by=4)
#nSlow<-26
nSig <- seq(from=3,to=15,by=3)
#nSig<-9
#nWait<-seq(from=0,to=3,by=2)
#lossLimits <- seq(from=100, to=2000, by=200)
#profitTarget <- seq(from=200, to=4000, by=400)

paramsList  <- list(lookbackS,lookbackL,lookbackR,threshold,
                    nFast,nSlow,nSig)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=8)
colnames(resultsMatrix) <- c("lookbackS","lookbackL","lookbackR",
                             "threshold","nFast","nSlow","nSig","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (lbs in lookbackS) {
  for (lbl in lookbackL) {
    for (lbr in lookbackR) {
      for (th in threshold) {
        for (nf in nFast) {
          for (ns in nSlow) {
            if(ns>nf){
              for (nsig in nSig){
                #for (ll in lossLimits){
                #for (pt in profitTarget){
                params <- list(lookbackS=lbs,lookbackL=lbl,lookbackR=lbr,
                               threshold=th,nFast=nf,nSlow=ns,nSig=nsig,
                               #lossLimits=ll,profitTarget=pt,
                               series=2,posSizes=rep(1,10)) 
                results <- backtest(dataList, getOrders, params, sMult)
                pfolioPnL <- plotResults(dataList,results)
                resultsMatrix[count,] <- c(lbs,lbl,lbr,th,nf,ns,nsig,
                                           #ll,pt,
                                           pfolioPnL$fitAgg)
                pfolioPnLList[[count]]<- pfolioPnL
                cat("Just completed",count,"out of",numberComb,"\n")
                print(resultsMatrix[count,])
                count <- count + 1
                #}
                #}
              }
            }
          }
        }
      }
    }
  }
}


print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])
