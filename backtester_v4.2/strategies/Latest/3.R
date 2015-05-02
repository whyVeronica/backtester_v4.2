#Crossover+Stoploss+target profit+ProfitPos
maxRows <- 2200
cashFlow <- cashFlow1 <- cashFlow2 <- rep(0,10)
#CPnL1 <- CPnL2 <- rep(0,10)
CPnL1 <- CPnL2 <- matrix(0,nrow=maxRows,ncol=10)
PnLCurrent1 <- PnLCurrent2 <- matrix(0,nrow=maxRows,ncol=10)

getOrders <- function(store, newRowList, currentPos, params) {
  #cat("DAY",store$iter,":","\n")
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) {
    store <- initStore(newRowList,params$series)
  }
  else 
    store <- updateStore(store, newRowList,params$series)
  
  positionSizes <- rep(1,length(newRowList))
  POS <- pos1 <- pos2 <- allzero
  PnLOld1 <- PnLOld2 <- allzero
  PnLNew1 <- PnLNew2 <- allzero
  posPnL <- rep(1,length(newRowList))
  currentCashFlow <- currentCashFlow11 <- currentCashFlow12 <- currentCashFlow21<- currentCashFlow22 <- allzero
  currentWorth1 <- currentWorth2 <- allzero
  MAPnL1 <- MAPnL2 <- allzero
  marketOrders <- marketOrders1 <- marketOrders2 <- allzero
  #maxLogRet <- allzero
  #spread <- allzero
  limitOrders1 <- allzero
  limitPrices1 <- allzero
  limitOrders2 <- allzero
  limitPrices2 <- allzero

#############################################################################################
  lookback1 <- params$lookbackL1 + round((params$lookbackL1+params$lookbackS1)/2)
  
  if (store$iter >= 
        max(params$lookbackWRL1,params$lookbackL1,params$lookbackS1,lookback1)) {

    startIndex <- store$iter - lookback1
    startIndexWRL <- store$iter - params$lookbackWRL1
    startIndexS <- store$iter - params$lookbackS1
    startIndexL <- store$iter - params$lookbackL1
    
    for (i in 1:length(params$series)) {
      posPnL[params$series[i]] <- ifelse(PnLCurrent1[store$iter-1,params$series[i]] > 0,
                                         PnLCurrent1[store$iter-1,params$series[i]],1)
      positionSizes[params$series[i]] <- round(positionSizes[params$series[i]]*posPnL[params$series[i]])
      
      cl <- newRowList[[params$series[i]]]$Close
      
      if(CPnL1[store$iter-1,params$series[i]] < -params$lossLimits1 
         || CPnL1[store$iter-1,params$series[i]] > params$profitTarget1) {
        marketOrders1[params$series[i]] <- -currentPos[params$series[i]]
        currentCashFlow11[params$series[i]] <- currentPos[params$series[i]]*cl
      }
      else
        marketOrders1[params$series[i]] <- 0
      
      highest <- max(store$hi[startIndexWRL:store$iter,i])
      lowest <- min(store$lo[startIndexWRL:store$iter,i])
      R <- (highest - cl)/(highest - lowest)*(-100)
      
      macdData <- MACD(store$cl[startIndex:store$iter,i],
                       nSlow=params$lookbackL1,nFast=params$lookbackS1,nSig=round((params$lookbackL1+params$lookbackS1)/2))
      macd <- macdData[,1]
      signal <- macdData[,2]
      
      MAclS <- SMA(store$cl[(startIndexS-1):store$iter,i],n=params$lookbackS1)
      MAclL <- SMA(store$cl[startIndexL:store$iter,i],n=params$lookbackL1)
      
      #Take a positin when signel line cross(as soon as the trend acceleration appears)
      #if (params$nWait == 0){
      #in a oversold/overbought price level
      if (R <= -50 - params$threshold1){
        #countOS[i] <<- countOS[i] + 1
        #when there's a bullish crossover
        if (last(MAclS) > last(MAclL) && last(MAclS,n=2)[-2] < last(MAclL,n=2)[-2] 
            || last(macd) > last(signal) && last(macd,n=2)[-2] < last(signal,n=2)[-2]){
          pos1[params$series[i]] <- positionSizes[params$series[i]] # long
          currentCashFlow12[params$series[i]] <- -cl*positionSizes[params$series[i]]
        }         
      }
      #When in overbought price level
      if (R > -50 + params$threshold1){
        #countOB[i] <<- countOB[i] + 1
        #when there's a bearish crossover
        if (last(MAclS) < last(MAclL) && last(MAclS,n=2)[-2] > last(MAclL,n=2)[-2] 
            || last(macd) < last(signal) && last(macd,n=2[-2]) > last(signal,n=2)[-2]){
          pos1[params$series[i]] <- -positionSizes[params$series[i]]  # short
          currentCashFlow12[params$series[i]] <- cl*positionSizes[params$series[i]]
        }
      }
      cashFlow1[params$series[i]] <<- cashFlow1[params$series[i]] + 
        currentCashFlow11[params$series[i]] + currentCashFlow12[params$series[i]]
      currentWorth1[params$series[i]] <- cl*currentPos[params$series[i]]
      PnLOld1[params$series[i]] <- CPnL1[store$iter,params$series[i]] #Yesterday's profit/loss
      PnLNew1[params$series[i]] <- currentWorth1[params$series[i]] +  cashFlow1[params$series[i]]
      PnLCurrent1[store$iter,params$series[i]] <<- PnLNew1[params$series[i]] - PnLOld1[params$series[i]]
      CPnL1[store$iter,params$series[i]] <- PnLNew1[params$series[i]]
      MAPnL1[params$series[i]] <- last(SMA(CPnL[startIndexWRL:store$iter,params$series[i]]))
      #print(MAPnL)
    }
  }
  
  #####################################################################################
  lookback2 <- params$lookbackL2 + round((params$lookbackL2+params$lookbackS2)/2)
  
  if (store$iter >= max(params$lookbackWRL2,params$lookbackL2,params$lookbackS2,lookback2)) {
    
    startIndex <- store$iter - lookback2
    startIndexWRL <- store$iter - params$lookbackWRL2
    startIndexS <- store$iter - params$lookbackS2
    startIndexL <- store$iter - params$lookbackL2
    
    for (i in 1:length(params$series)) {
      cl <- newRowList[[params$series[i]]]$Close
      
      if(CPnL2[store$iter-1,params$series[i]] < -params$lossLimits2 
         || CPnL2[store$iter-1,params$series[i]] > params$profitTarget2) {
        marketOrders2[params$series[i]] <- -currentPos[params$series[i]]
        currentCashFlow21[params$series[i]] <- currentPos[params$series[i]]*cl
      }
      else
        marketOrders2[params$series[i]] <- 0
      
      posPnL[params$series[i]] <- ifelse(PnLCurrent2[store$iter-1,params$series[i]] > 0,
                                         PnLCurrent2[store$iter-1,params$series[i]],1)
      positionSizes[params$series[i]] <- round(positionSizes[params$series[i]]*posPnL[params$series[i]])
      
      highest <- max(store$hi[startIndexWRL:store$iter,i])
      lowest <- min(store$lo[startIndexWRL:store$iter,i])
      R <- (highest - cl)/(highest - lowest)*(-100)
      #cat("W%R: ",R,"\n")
      
      macdData <- MACD(store$cl[startIndex:store$iter,i],
                       nSlow=params$lookbackL2,nFast=params$lookbackS2,nSig=round((params$lookbackL2+params$lookbackS2)/2))
      macd <- macdData[,1]
      signal <- macdData[,2]
      
      MAclS <- SMA(store$cl[(startIndexS-1):store$iter,i],n=params$lookbackS2)
      MAclL <- SMA(store$cl[startIndexL:store$iter,i],n=params$lookbackL2)
      
      #Take a positin when signel line cross(as soon as the trend acceleration appears)
      #if (params$nWait == 0){
      #in a oversold/overbought price level
      if (R < -50 - params$threshold2){
        #countOS[i] <<- countOS[i] + 1
        #when there's a bullish crossover
        if (last(MAclS) > last(MAclL) && last(MAclS,n=2)[-2] < last(MAclL,n=2)[-2] 
            || last(macd) > last(signal) && last(macd,n=2)[-2] < last(signal,n=2)[-2]){
          pos2[params$series[i]] <- positionSizes[params$series[i]] # long
          currentCashFlow22[params$series[i]] <- -cl*positionSizes[params$series[i]]
        }         
      }
      
      #When in overbought price level
      if (R > -50 + params$threshold2){
        #countOB[i] <<- countOB[i] + 1
        #when there's a bearish crossover
        if (last(MAclS) < last(MAclL) && last(MAclS,n=2)[-2] > last(MAclL,n=2)[-2] 
            || last(macd) < last(signal) && last(macd,n=2[-2]) > last(signal,n=2)[-2]){
          pos2[params$series[i]] <- -positionSizes[params$series[i]]  # short
          currentCashFlow22[params$series[i]] <- cl*positionSizes[params$series[i]]
        }
      }
      currentWorth2[params$series[i]] <- cl*currentPos[params$series[i]]
      cashFlow2[params$series[i]] <<- cashFlow2[params$series[i]] + 
        currentCashFlow21[params$series[i]] + currentCashFlow22[params$series[i]]
      PnLOld2[params$series[i]] <- CPnL2[store$iter,params$series[i]] #Yesterday's profit/loss
      PnLNew2[params$series[i]] <- currentWorth2[params$series[i]] +  cashFlow2[params$series[i]]
      PnLCurrent2[store$iter,params$series[i]] <<- PnLNew2[params$series[i]] - PnLOld2[params$series[i]]
      CPnL2[store$iter,params$series[i]] <- PnLNew2[params$series[i]]
      MAPnL2[params$series[i]] <- last(SMA(CPnL[startIndexWRL:store$iter,params$series[i]]))
    }
  }
  ###########################################################################################
  #cat("marketOrders before: ",marketOrders,"\n")
  
  #POS <- sapply(1:params$series[i],function(i))
  for(i in 1:length(params$series)){ 
    if(MAPnL1[params$series[i]] >= MAPnL2[params$series[i]]){
      POS[params$series[i]] <- pos1[params$series[i]]
      marketOrders[params$series[i]] <- marketOrders1[params$series[i]]
      currentCashFlow[params$series[i]] <- currentCashFlow11[params$series[i]] + currentCashFlow12[params$series[i]]
    }
    else {
      POS[params$series[i]] <- pos2[params$series[i]]
      marketOrders[params$series[i]] <- marketOrders2[params$series[i]]
      currentCashFlow[params$series[i]] <- currentCashFlow21[params$series[i]] + currentCashFlow22[params$series[i]]
    }
  }
  
  marketOrders <- marketOrders + POS
  cashFlow <<- cashFlow + currentCashFlow
  #CPnL[store$iter,] <<- PnLNew
  #cat("PnL: ",PnL,"\n")
  #POS <<- positionSizes
  
  #cat("marketOrders: ",marketOrders,"\n")
  #cat("currentPos: ",currentPos,"\n")
  #cat("Oversold times: ",countOS,"\n")
  #cat("Overbought times: ",countOB,"\n")
  #cat("currentCashFlow: ",currentCashFlow,"\n")
  #cat("current worth: ",currentWorth,"\n")
  #cat("PnL: ",PnL,"\n")
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
              op=initOpStore(newRowList,series)
              #,CF=initCFStore(newRowList,series)
  ))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$op <- updateOpStore(store$op,newRowList,series,store$iter)
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  store$hi <- updateHiStore(store$hi,newRowList,series,store$iter)
  store$lo <- updateLoStore(store$lo,newRowList,series,store$iter)
  return(store)
}