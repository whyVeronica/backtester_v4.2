source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/Latest/Williams%R & MA.R') 

numOfDays <- 1100
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

lookbackR <- seq(from=5, to=15, by=5)
lookbackS <- seq(from=1, to=7,by=4)
lookbackL <- seq(from=8, to=16, by=8)
threshold<-seq(from=20,to=45,by=10)
#threshold<-30
#nWait<-seq(from=0,to=3,by=2)
nWait <- 0

paramsList  <- list(lookbackS,lookbackL,lookbackR,threshold,nWait)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=6)
colnames(resultsMatrix) <- c("lookbackS","lookbackL","lookbackR","threshold","nWait","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (lbs in lookbackS) {
  for (lbl in lookbackL) {
    for (lbr in lookbackR) {
      for (th in threshold) {
            for (nw in nWait) {
                  for (nw in nWait){
                    params <- list(lookbackS=lbs,lookbackL=lbl,lookbackR=lbr,threshold=th,
                                   nWait=nw,series=1,posSizes=rep(1,10)) 
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
