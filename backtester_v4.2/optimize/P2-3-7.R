source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/W%R-MACD-MA3-limit1.R') 

numOfDays <- 1100
dataList <- getData(directory="PART2")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

#params <- list(lookbackR=14,lookbackS=5,lookbackL=8,threshold=40,nWait=0,
#nFast=5, nSlow=10, nSig=5, 
#lossLimits=8000, profitTarget=10000, series=c(1:10))#W%R & macd 

lookbackR <- seq(from=10, to=20, by=10)
lookbackS <- seq(from=5, to=10,by=5)
lookbackL <- seq(from=10, to=50, by=20)
threshold <- seq(from=20,to=40,by=10)
#threshold<-30
nFast <- seq(from=5,to=15,by=10)
#nFast<-12
nSlow <- seq(from=10,to=50,by=20)
#nSlow<-26
nSig <- seq(from=5,to=20,by=10)
#nSig<-9
#nWait<-seq(from=0,to=3,by=2)
lossLimits <- seq(from=50,to=550,by=500)
profitTarget <- seq(from=50,to=550,by=500)
#lookbackLimit <- seq(from=10,to=30,by=10)
lookbackLimit <- 14

paramsList  <- list(lookbackS,lookbackL,lookbackR,threshold,
                    nFast,nSlow,nSig,lookbackLimit,lossLimits,profitTarget)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=11)
colnames(resultsMatrix) <- c("lookbackS","lookbackL","lookbackR",
                             "threshold","nFast","nSlow","nSig",
                             "lossLimits","profitTarget","lookbackLimit","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (lbs in lookbackS) {
  for (lbl in lookbackL) {
    if(lbs < lbl){
      for (lbr in lookbackR) {
        for (th in threshold) {
          for (nf in nFast) {
            for (ns in nSlow) {
              if(ns>nf){
                for (nsig in nSig){
                  for(lbli in lookbackLimit){
                    for (ll in lossLimits){
                      for (pt in profitTarget){
                        params <- list(lookbackS=lbs,lookbackL=lbl,lookbackR=lbr,
                                       threshold=th,nFast=nf,nSlow=ns,nSig=nsig,
                                       lookbackLimit=lbli,
                                       lossLimits=ll,profitTarget=pt,
                                       series=7,posSizes=rep(1,10)) 
                        results <- backtest(dataList, getOrders, params, sMult)
                        pfolioPnL <- plotResults(dataList,results)
                        resultsMatrix[count,] <- c(lbs,lbl,lbr,th,nf,ns,nsig,lbli,ll,pt,pfolioPnL$fitAgg)
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
        }
      }
    }
    
  }
}


print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])
