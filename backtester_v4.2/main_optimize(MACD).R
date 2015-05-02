source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/Latest/MACD(posSizes-mean abs diff).R') 

numOfDays <- 1100
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

#params <- list(nFast=5, nSlow=10, nSig=5,nWait=0, series=c(1:10))#MACD

nFast<-seq(from=3,to=10,by=3)
#nFast<-12
nSlow<-seq(from=8,to=26,by=8)
#nSlow<-26
nSig<-seq(from=3,to=15,by=5)
#nSig<-9
#nWait<-seq(from=0,to=3,by=2)
nWait <- 0
#lookback <- nSlow + nSig + nWait

paramsList  <- list(nFast,nSlow,nSig,nWait)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=5)
colnames(resultsMatrix) <- c("nFast","nSlow","nSig","nWait","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1

        for (nf in nFast) {
          for (ns in nSlow) {
            if(ns>nf){
              for (nsig in nSig){
                for (nw in nWait){
                    params <- list(nFast=nf,nSlow=ns,nSig=nsig,nw=nWait,series=1,posSizes=rep(1,10)) 
                    results <- backtest(dataList, getOrders, params, sMult)
                    pfolioPnL <- plotResults(dataList,results)
                    resultsMatrix[count,] <- c(nf,ns,nsig,nw,pfolioPnL$fitAgg)
                    pfolioPnLList[[count]]<- pfolioPnL
                    cat("Just completed",count,"out of",numberComb,"\n")
                    print(resultsMatrix[count,])
                    count <- count + 1
                  }
                }
              }
            }
          }


print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])
