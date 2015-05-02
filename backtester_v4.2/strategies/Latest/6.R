#Crossover+Stoploss+target profit+ProfitPos
maxRows <- 2200
#CPnL1 <- CPnL2 <- rep(0,10)
CPnL1 <- CPnL2 <- matrix(0,nrow=maxRows,ncol=10)
PnLCurrent1 <- PnLCurrent2 <- matrix(0,nrow=maxRows,ncol=10)
currentCashFlow <-  currentCashFlow11 <- currentCashFlow12 <- currentCashFlow21<- currentCashFlow22 <- matrix(0,nrow=maxRows,ncol=10)
cashFlow <- cashFlow1 <- cashFlow2 <- matrix(0,nrow=maxRows,ncol=10)
currentPos1 <- currentPos2 <- rep(0,10)
maxDrawdown1 <- maxDrawdown2 <- matrix(0,nrow=maxRows,ncol=10)

getOrders <- function(store, newRowList, currentPos, params) {
  cat("DAY",store$iter,":","\n")
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) {
    store <- initStore(newRowList,params$series)
  }
  else 
    store <- updateStore(store, newRowList,params$series)
  
  positionSizes <- rep(1,length(newRowList))
  POS <- pos1 <- pos2 <- allzero
  #PnLOld1 <- PnLOld2 <- allzero
  PnLNew <- PnLNew1 <- PnLNew2 <- allzero
  posPnL <- rep(1,length(newRowList))
  
  currentWorth <- currentWorth1 <- currentWorth2 <- allzero
  MAPnL1 <- MAPnL2 <- allzero
  marketOrders <- marketOrders1 <- marketOrders2 <- allzero
  #maxLogRet <- allzero
  #spread <- allzero
  limitOrders1 <- allzero
  limitPrices1 <- allzero
  limitOrders2 <- allzero
  limitPrices2 <- allzero
  
  ##########################################################################################################
  lookback1 <- params$lookbackL1 + round((params$lookbackL1+params$lookbackS1)/2)
  maxLookback1 <- max(params$lookbackWRL1,params$lookbackL1,params$lookbackS1,lookback1)
  lookback2 <- params$lookbackL2 + round((params$lookbackL2+params$lookbackS2)/2)
  maxLookback2 <- max(params$lookbackWRL2,params$lookbackL2,params$lookbackS2,lookback2)
  
  #cat("cashflow: ",cashFlow,"\n")
  
  if (store$iter > max(maxLookback1,maxLookback2)) {
    startIndex1 <- store$iter - lookback1
    startIndexWRL1 <- store$iter - params$lookbackWRL1
    startIndexS1 <- store$iter - params$lookbackS1
    startIndexL1 <- store$iter - params$lookbackL1
    
    startIndex2 <- store$iter - lookback2
    startIndexWRL2 <- store$iter - params$lookbackWRL2
    startIndexS2 <- store$iter - params$lookbackS2
    startIndexL2 <- store$iter - params$lookbackL2
    
    for (i in 1:length(params$series)) {
      #posPnL[i] <- ifelse(PnLCurrent[store$iter,i] > 0,PnLCurrent[store$iter,i],1)
      #posPnL[i] <- ifelse(posPnL[i]*0.01 > 1,posPnL[i]*0.1,1) #scaling down
      
      #openDiffs <- diff(store$op)
      #absOpenDiffs <- as.matrix(abs(openDiffs))
      #absOpenDiffs <- absOpenDiffs[1:store$iter,]
      #avgAbsDiffs <- colMeans(absOpenDiffs)
      #largestAvgAbsDiff <- max(avgAbsDiffs)
      #positionSizes[i] <- round(largestAvgAbsDiff/avgAbsDiffs[i])
      #positionSizes[i] <- round(positionSizes[i]*posPnL[i])
      #print(store$cl)
      
      #currentWorth[params$series[i]] <- cl*currentPos[params$series[i]] 
      cl <- newRowList[[params$series[i]]]$Close
      
      #cat("CPnL1 before:",CPnL1[(store$iter-1),params$series[i]],"\n")
      #cat("currentPos",currentPos,"\n")
      
      #if(CPnL1[(store$iter-1),params$series[i]] < -params$lossLimits1 
      #   || CPnL1[(store$iter-1),params$series[i]] > params$profitTarget1) {
      #  marketOrders1[params$series[i]] <- -currentPos[params$series[i]]
      #  currentCashFlow11[store$iter,params$series[i]] <- currentPos[params$series[i]]*cl
      #  cat("marketOrders1:",marketOrders1,"\n")
      #  cat("currentCashFlow11:",currentCashFlow11[store$iter,params$series[i]],"\n")
      #cashFlow[params$series[i]] <- cashFlow[params$series[i]] + currentCashFlow[params$series[i]]
      #}
      #else {
      #  marketOrders1[params$series[i]] <- 0
      #}
      
      
      highest1 <- max(store$hi[startIndexWRL1:store$iter,i])
      lowest1 <- min(store$lo[startIndexWRL1:store$iter,i])
      R1 <- (highest1 - cl)/(highest1 - lowest1)*(-100)
      #cat("W%R: ",R,"\n")
      
      macdData1 <- MACD(store$cl[startIndex1:store$iter,i],
                        nSlow=params$lookbackL1,nFast=params$lookbackS1,nSig=round((params$lookbackL1+params$lookbackS1)/2))
      macd1 <- macdData1[,1]
      signal1 <- macdData1[,2]
      
      MAclS1 <- SMA(store$cl[(startIndexS1-1):store$iter,i],n=params$lookbackS1)
      MAclL1 <- SMA(store$cl[startIndexL1:store$iter,i],n=params$lookbackL1)
      
      #Take a positin when signel line cross(as soon as the trend acceleration appears)
      #if (params$nWait == 0){
      #in a oversold/overbought price level
      if (R1 < -50 - params$threshold1){
        #countOS[i] <<- countOS[i] + 1
        #when there's a bullish crossover
        if (last(MAclS1) > last(MAclL1) && last(MAclS1,n=2)[-2] < last(MAclL1,n=2)[-2] 
            || last(macd1) > last(signal1) && last(macd1,n=2)[-2] < last(signal1,n=2)[-2]){
          pos1[params$series[i]] <- positionSizes[params$series[i]] # long
          currentCashFlow12[store$iter,params$series[i]] <- -cl*positionSizes[params$series[i]]
          #cat("currentCashFlow12:",currentCashFlow12[store$iter,params$series[i]],"\n")
        }         
      }
      #When in overbought price level
      if (R1 > -50 + params$threshold1){
        #countOB[i] <<- countOB[i] + 1
        #when there's a bearish crossover
        if (last(MAclS1) < last(MAclL1) && last(MAclS1,n=2)[-2] > last(MAclL1,n=2)[-2] 
            || last(macd1) < last(signal1) && last(macd1,n=2[-2]) > last(signal1,n=2)[-2]){
          pos1[params$series[i]] <- -positionSizes[params$series[i]]  # short
          currentCashFlow12[store$iter,params$series[i]] <- cl*positionSizes[params$series[i]]
          #cat("currentCashFlow12:",currentCashFlow12[store$iter,params$series[i]],"\n")
        }
      }
      #print(cashFlow1[store$iter-1,params$series[i]])
      #print(cashFlow1[store$iter,params$series[i]])
      #print(currentCashFlow12[store$iter,params$series[i]])
      #cat("cashFlow1 before:",cashFlow1[(store$iter-1),params$series[i]],"\n")
      marketOrders1[params$series[i]] <- marketOrders1[params$series[i]] + pos1[params$series[i]]
      currentPos1[params$series[i]] <<- currentPos1[params$series[i]] + marketOrders1[params$series[i]]
      cashFlow1[store$iter,params$series[i]] <<- cashFlow1[(store$iter-1),params$series[i]] + currentCashFlow11[store$iter,params$series[i]] + currentCashFlow12[store$iter,params$series[i]]
      #cat("cashFlow1:",cashFlow1[store$iter,params$series[i]],"\n")
      #PnLOld[params$series[i]] <- PnL[params$series[i]] #Yesterday's profit/loss
      currentWorth1[params$series[i]] <- cl*currentPos1[params$series[i]]
      PnLNew1[params$series[i]] <- currentWorth1[params$series[i]] +  cashFlow1[store$iter,params$series[i]]
      #cat("PnLNew1[params$series[i]]",PnLNew1[params$series[i]],"\n")
      #PnLCurrent[store$iter,params$series[i]] <<- PnLNew[params$series[i]] - PnLOld[params$series[i]]
      CPnL1[store$iter,params$series[i]] <<- PnLNew1[params$series[i]]
      MAPnL1[params$series[i]] <- last(SMA(CPnL1[startIndexWRL1:store$iter,params$series[i]]))
      
      cumPnL1 <<- CPnL1[1:store$iter,params$series[i]]
      #cat("cumPnL1:",CPnL1[1:store$iter,params$series[i]],"\n")
      #cat("MAX cumPnL1:",max(cumPnL1),"\n")
      maxDrawdown1[store$iter,params$series[i]] <<- max(max(cumPnL1)-cumPnL1,maxDrawdown1[store$iter-1,params$series[i]])
      #cat("maxDrawdown1",maxDrawdown1[store$iter,],"\n")
      PD1 <- ifelse(CPnL1[store$iter,params$series[i]] > 0, 
                    CPnL1[store$iter,params$series[i]]/maxDrawdown1[store$iter,params$series[i]], 
                    CPnL1[store$iter,params$series[i]])
      #cat("PD1:",PD1,"\n")
      ######################################################################################################
      #if(CPnL2[(store$iter-1),params$series[i]] < -params$lossLimits2
      #   || CPnL2[(store$iter-1),params$series[i]] > params$profitTarget2) {
      #  marketOrders2[params$series[i]] <- -currentPos[params$series[i]]
      #  currentCashFlow21[store$iter,params$series[i]] <- currentPos[params$series[i]]*cl
      #  #cashFlow[params$series[i]] <- cashFlow[params$series[i]] + currentCashFlow[params$series[i]]
      #}
      #else {
      #  marketOrders2[params$series[i]] <- 0
      #}
      
      
      #posPnL[i] <- ifelse(PnLCurrent[store$iter,i] > 0,PnLCurrent[store$iter,i],1)
      #posPnL[i] <- ifelse(posPnL[i]*0.01 > 1,posPnL[i]*0.1,1) #scaling down
      
      #openDiffs <- diff(store$op)
      #absOpenDiffs <- as.matrix(abs(openDiffs))
      #absOpenDiffs <- absOpenDiffs[1:store$iter,]
      #avgAbsDiffs <- colMeans(absOpenDiffs)
      #largestAvgAbsDiff <- max(avgAbsDiffs)
      #positionSizes[i] <- round(largestAvgAbsDiff/avgAbsDiffs[i])
      #positionSizes[i] <- round(positionSizes[i]*posPnL[i])
      #print(store$cl)
      
      highest2 <- max(store$hi[startIndexWRL2:store$iter,i])
      lowest2 <- min(store$lo[startIndexWRL2:store$iter,i])
      R2 <- (highest2 - cl)/(highest2 - lowest2)*(-100)
      #cat("W%R: ",R,"\n")
      
      macdData2 <- MACD(store$cl[startIndex2:store$iter,i],
                        nSlow=params$lookbackL2,nFast=params$lookbackS2,
                        nSig=round((params$lookbackL2+params$lookbackS2)/2))
      macd2 <- macdData2[,1]
      signal2 <- macdData2[,2]
      
      MAclS2 <- SMA(store$cl[(startIndexS2-1):store$iter,i],n=params$lookbackS2)
      MAclL2 <- SMA(store$cl[startIndexL2:store$iter,i],n=params$lookbackL2)
      
      #Take a positin when signel line cross(as soon as the trend acceleration appears)
      #if (params$nWait == 0){
      #in a oversold/overbought price level
      if (R2 < -50 - params$threshold2){
        #countOS[i] <<- countOS[i] + 1
        #when there's a bullish crossover
        if (last(MAclS2) > last(MAclL2) && last(MAclS2,n=2)[-2] < last(MAclL2,n=2)[-2] 
            || last(macd2) > last(signal2) && last(macd2,n=2)[-2] < last(signal2,n=2)[-2]){
          pos2[params$series[i]] <- positionSizes[params$series[i]] # long
          currentCashFlow22[store$iter,params$series[i]] <- -cl*positionSizes[i]
        }         
      }
      #When in overbought price level
      if (R2 > -50 + params$threshold2){
        #countOB[i] <<- countOB[i] + 1
        #when there's a bearish crossover
        if (last(MAclS2) < last(MAclL2) && last(MAclS2,n=2)[-2] > last(MAclL2,n=2)[-2] 
            || last(macd2) < last(signal2) && last(macd2,n=2[-2]) > last(signal2,n=2)[-2]){
          pos2[params$series[i]] <- -positionSizes[params$series[i]]  # short
          currentCashFlow22[store$iter,params$series[i]] <- cl*positionSizes[i]
        }
      } 
      marketOrders2[params$series[i]] <- marketOrders2[params$series[i]] + pos2[params$series[i]]
      currentPos2[params$series[i]] <<- currentPos2[params$series[i]] + marketOrders2[params$series[i]]
      
      cashFlow2[store$iter,params$series[i]] <<- cashFlow2[(store$iter-1),params$series[i]] 
      + currentCashFlow21[store$iter,params$series[i]] + currentCashFlow22[store$iter,params$series[i]]
      #cat("cashFlow2:",cashFlow2[store$iter,params$series[i]],"\n")
      #PnLOld[params$series[i]] <- PnL[params$series[i]] #Yesterday's profit/loss
      currentWorth2[params$series[i]] <- cl*currentPos2[params$series[i]]
      PnLNew2[params$series[i]] <- currentWorth2[params$series[i]] +  cashFlow2[params$series[i]]
      #cat("PnLNew2[params$series[i]]",PnLNew2[params$series[i]],"\n")
      #PnLCurrent[store$iter,params$series[i]] <<- PnLNew[params$series[i]] - PnLOld[params$series[i]]
      CPnL2[store$iter,params$series[i]] <<- PnLNew2[params$series[i]]
      #cat("CPnL1[startIndexWRL1:store$iter,params$series[i]]:",CPnL1[startIndexWRL1:store$iter,params$series[i]],"\n")
      #cat("CPnL2[startIndexWRL2:store$iter,params$series[i]]:",CPnL2[startIndexWRL2:store$iter,params$series[i]],"\n")
      MAPnL2[params$series[i]] <- last(SMA(CPnL2[startIndexWRL2:store$iter,params$series[i]]))
      
      cumPnL2 <<- CPnL2[1:store$iter,params$series[i]]
      #cat("cumPnL2:",CPnL2[1:store$iter,params$series[i]],"\n")
      #cat("MAX cumPnL2:",max(cumPnL2),"\n")
      maxDrawdown2[store$iter,params$series[i]] <<- max(max(cumPnL2)-cumPnL2,maxDrawdown2[store$iter-1,params$series[i]])
      #cat("maxDrawdown2",maxDrawdown2[store$iter,],"\n")
      PD2 <- ifelse(CPnL2[store$iter,params$series[i]] > 0, 
                    CPnL2[store$iter,params$series[i]]/maxDrawdown2[store$iter,params$series[i]], 
                    CPnL2[store$iter,params$series[i]])
      #cat("PD2:",PD2,"\n")
      ###########################################################################################
      #cat("MAPnL1:",MAPnL1,"\n")
      #cat("MAPnL2:",MAPnL2,"\n")
      cat("PD1:",PD1,"\n")
      cat("PD2:",PD2,"\n")
      ###########################################################################################
      if(PD1 >= PD2){
        #POS[params$series[i]] <- pos1[params$series[i]]
        marketOrders[params$series[i]] <- marketOrders1[params$series[i]]
        currentCashFlow[store$iter,params$series[i]] <- currentCashFlow11[store$iter,params$series[i]] + currentCashFlow12[store$iter,params$series[i]]
      }
      else {
        #POS[params$series[i]] <- pos2[params$series[i]]
        marketOrders[params$series[i]] <- marketOrders2[params$series[i]]
        currentCashFlow[store$iter,params$series[i]] <- currentCashFlow21[store$iter,params$series[i]] + currentCashFlow22[store$iter,params$series[i]]
      }
    }
  }
  
  ############################################################################################################
  
  #cat("marketOrders before: ",marketOrders,"\n")
  #marketOrders <- marketOrders1
  #marketOrders <- marketOrders2
  #cat("marketOrders after:",marketOrders,"\n")
  ###marketOrders <- marketOrders + POS
  
  #cashFlow <<- cashFlow1 + currentCashFlow11 + currentCashFlow12
  #cashFlow <<- cashFlow2 + currentCashFlow21 + currentCashFlow22
  #cashFlow <<- cashFlow + currentCashFlow
  
  #cat("marketOrders1: ",marketOrders1,"\n")
  #cat("currentPos1: ",currentPos1,"\n")
  #cat("Oversold times: ",countOS,"\n")
  #cat("Overbought times: ",countOB,"\n")
  #cat("currentCashFlow: ",currentCashFlow,"\n")
  #cat("current worth1: ",currentWorth1,"\n")
  #cat("CPnL1: ",CPnL1[store$iter,],"\n")
  #cat("PnL Current: ",PnLCurrent[store$iter,],"\n")
  #cat("count: ",count,"\n")
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,
              limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,
              limitPrices2=limitPrices2))
}

initOpStore  <- function(newRowList,series) {
  opStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(opStore)
}
initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
initHiStore  <- function(newRowList,series) {
  hiStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(hiStore)
}
initLoStore  <- function(newRowList,series) {
  loStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(loStore)
}

updateOpStore <- function(opStore, newRowList, series, iter) {
  for (i in 1:length(series))
    opStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Open)
  return(opStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}
updateHiStore <- function(hiStore, newRowList, series, iter) {
  for (i in 1:length(series))
    hiStore[iter,i] <- as.numeric(newRowList[[series[i]]]$High)
  return(hiStore)
}
updateLoStore <- function(loStore, newRowList, series, iter) {
  for (i in 1:length(series))
    loStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Low)
  return(loStore)
}

initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),
              hi=initHiStore(newRowList,series),
              lo=initLoStore(newRowList,series),
              op=initOpStore(newRowList,series)))
}

updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$op <- updateOpStore(store$op,newRowList,series,store$iter)
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  store$hi <- updateHiStore(store$hi,newRowList,series,store$iter)
  store$lo <- updateLoStore(store$lo,newRowList,series,store$iter)
  return(store)
} 