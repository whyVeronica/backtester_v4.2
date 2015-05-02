#Crossover+Stoploss+target profit+ProfitPos
maxRows <- 2200
#cashFlow <- rep(0,10)
#PnL <- rep(0,10)
#PnLCurrent <- matrix(0,nrow=maxRows,ncol=10)
#POS <- rep(1,10)
#countOS <- rep(0,10)
#countOB <- rep(0,10)
#count <- rep(0,10)


getOrders <- function(store, newRowList, currentPos, params1, params2) {
  #cat("DAY",store$iter,":","\n")
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) {
    #checkparams1(params1)
    #cashFlow <- rep(0,10)
    #PnL <- rep(0,10)
    store <- initStore(newRowList,params1$series)
  }
  else 
    store <- updateStore(store, newRowList,params1$series)
  
  positionSizes <- rep(1,length(newRowList))
  pos1 <- allzero
  pos2 <- allzero
  #PnLOld <- allzero
  #PnLNew <- allzero
  #currentCashFlow <- allzero
  #currentWorth <- allzero
  marketOrders <- allzero
  #posPnL <- rep(1,10)
  maxLogRet <- allzero
  spread <- allzero
  limitOrders1 <- allzero
  limitPrices1 <- allzero
  limitOrders2 <- allzero
  limitPrices2 <- allzero
  
  lookback1 <- params1$lookbackL + round((params1$lookbackL+params1$lookbackS)/2)
  
  #cat("cashflow: ",cashFlow,"\n")
  
  if (store$iter >= max(params1$lookbackWRL,params1$lookbackL,params1$lookbackS,lookback1)) {
    startIndex <- store$iter - lookback1
    startIndexWRL <- store$iter - params1$lookbackWRL
    startIndexS <- store$iter - params1$lookbackS
    startIndexL <- store$iter - params1$lookbackL
    
    for (i in 1:length(params1$series)) {
      #marketOrders[i] <- ifelse(PnL[i] < -params1$lossLimits || PnL[i] > params1$profitTarget, -currentPos[i], 0)
      
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
      cl <- newRowList[[params1$series[i]]]$Close
      highest <- max(store$hi[startIndexWRL:store$iter,i])
      lowest <- min(store$lo[startIndexWRL:store$iter,i])
      R <- (highest - cl)/(highest - lowest)*(-100)
      #cat("W%R: ",R,"\n")
      
      macdData <- MACD(store$cl[startIndex:store$iter,i],
                       nSlow=params1$lookbackL,nFast=params1$lookbackS,nSig=round((params1$lookbackL+params1$lookbackS)/2))
      macd <- macdData[,1]
      signal <- macdData[,2]
      
      MAclS <- SMA(store$cl[(startIndexS-1):store$iter,i],n=params1$lookbackS)
      MAclL <- SMA(store$cl[startIndexL:store$iter,i],n=params1$lookbackL)
      
      #Take a positin when signel line cross(as soon as the trend acceleration appears)
      #if (params1$nWait == 0){
      #in a oversold/overbought price level
      if (R < -50 - params1$threshold){
        #countOS[i] <<- countOS[i] + 1
        #when there's a bullish crossover
        if (last(MAclS) > last(MAclL) && last(MAclS,n=2)[-2] < last(MAclL,n=2)[-2] 
            || last(macd) > last(signal) && last(macd,n=2)[-2] < last(signal,n=2)[-2]){
          pos1[params1$series[i]] <- positionSizes[params1$series[i]] # long
        }         
      }
      #When in overbought price level
      if (R > -50 + params1$threshold){
        #countOB[i] <<- countOB[i] + 1
        #when there's a bearish crossover
        if (last(MAclS) < last(MAclL) && last(MAclS,n=2)[-2] > last(MAclL,n=2)[-2] 
            || last(macd) < last(signal) && last(macd,n=2[-2]) > last(signal,n=2)[-2]){
          pos1[params1$series[i]] <- -positionSizes[params1$series[i]]  # short
        }
      }
      
      startIndexLimit <- store$iter - params1$lookbackWRL
      highest <- max(store$hi[startIndexLimit:store$iter,i])
      lowest <- min(store$lo[startIndexLimit:store$iter,i])
      maxLogRet[params1$series[i]] <- max(abs(diff(log(store$cl[startIndexLimit:store$iter,i]))))
      #print(maxLogRet)
      
      
      spread[params1$series[i]] <- maxLogRet[params1$series[i]] *10000* (highest - lowest)
      
      limitOrders1[params1$series[i]]  <- positionSizes[params1$series[i]] # BUY LIMIT ORDERS
      limitPrices1[params1$series[i]]  <- newRowList[[params1$series[i]]]$Close - spread[params1$series[i]]/2
      
      limitOrders2[params1$series[i]]  <- -positionSizes[params1$series[i]] # SELL LIMIT ORDERS
      limitPrices2[params1$series[i]]  <- newRowList[[params1$series[i]]]$Close + spread[params1$series[i]]/2
      #}
      
      #currentWorth[params1$series[i]] <- ifelse(store$iter == max(params1$lookbackR,lookback),
      #cl*pos[params1$series[i]],
      #cl*currentPos[params1$series[i]]) 
      #currentWorth[params1$series[i]] <- cl*currentPos[params1$series[i]]
      #PnLOld[params1$series[i]] <- PnL[params1$series[i]] #Yesterday's profit/loss
      #PnLNew[params1$series[i]] <- currentWorth[params1$series[i]] +  cashFlow[params1$series[i]]
      #PnLCurrent[store$iter,params1$series[i]] <<- PnLNew[params1$series[i]] - PnLOld[params1$series[i]]
      
      #if (marketOrders[i] != 0){
      #count[i] <<- count[i] + 1
      #}
      
    }
  }
  
  lookback2 <- params2$lookbackL + round((params2$lookbackL+params2$lookbackS)/2)

  if (store$iter >= max(params2$lookbackWRL,params2$lookbackL,params2$lookbackS,lookback2)) {
    startIndex <- store$iter - lookback2
    startIndexWRL <- store$iter - params2$lookbackWRL
    startIndexS <- store$iter - params2$lookbackS
    startIndexL <- store$iter - params2$lookbackL
    
    for (i in 1:length(params2$series)) {
      cl <- newRowList[[params2$series[i]]]$Close
      highest <- max(store$hi[startIndexWRL:store$iter,i])
      lowest <- min(store$lo[startIndexWRL:store$iter,i])
      R <- (highest - cl)/(highest - lowest)*(-100)
      #cat("W%R: ",R,"\n")
      
      macdData <- MACD(store$cl[startIndex:store$iter,i],
                       nSlow=params2$lookbackL,nFast=params2$lookbackS,nSig=round((params2$lookbackL+params2$lookbackS)/2))
      macd <- macdData[,1]
      signal <- macdData[,2]
      
      MAclS <- SMA(store$cl[(startIndexS-1):store$iter,i],n=params2$lookbackS)
      MAclL <- SMA(store$cl[startIndexL:store$iter,i],n=params2$lookbackL)
      
      #Take a positin when signel line cross(as soon as the trend acceleration appears)
      #if (params2$nWait == 0){
      #in a oversold/overbought price level
      if (R < -50 - params2$threshold){
        #countOS[i] <<- countOS[i] + 1
        #when there's a bullish crossover
        if (last(MAclS) > last(MAclL) && last(MAclS,n=2)[-2] < last(MAclL,n=2)[-2] 
            || last(macd) > last(signal) && last(macd,n=2)[-2] < last(signal,n=2)[-2]){
          pos2[params2$series[i]] <- positionSizes[params2$series[i]] # long
        }         
      }
      
      #When in overbought price level
      if (R > -50 + params2$threshold){
        #countOB[i] <<- countOB[i] + 1
        #when there's a bearish crossover
        if (last(MAclS) < last(MAclL) && last(MAclS,n=2)[-2] > last(MAclL,n=2)[-2] 
            || last(macd) < last(signal) && last(macd,n=2[-2]) > last(signal,n=2)[-2]){
          pos2[params2$series[i]] <- -positionSizes[params2$series[i]]  # short
        }
      }
      
      startIndexLimit <- store$iter - params2$lookbackWRL
      highest <- max(store$hi[startIndexLimit:store$iter,i])
      lowest <- min(store$lo[startIndexLimit:store$iter,i])
      maxLogRet[params2$series[i]] <- max(abs(diff(log(store$cl[startIndexLimit:store$iter,i]))))
      spread <- allzero
      
      spread[params2$series[i]] <- maxLogRet[params2$series[i]] *10000* (highest - lowest)
      
      
      limitOrders1[params2$series[i]]  <- positionSizes[params2$series[i]] # BUY LIMIT ORDERS
      limitPrices1[params2$series[i]]  <- newRowList[[params2$series[i]]]$Close - spread[params2$series[i]]/2
      
      limitOrders2[params2$series[i]]  <- -positionSizes[params2$series[i]] # SELL LIMIT ORDERS
      limitPrices2[params2$series[i]]  <- newRowList[[params2$series[i]]]$Close + spread[params2$series[i]]/2
      
    }
  }
  #cat("marketOrders before: ",marketOrders,"\n")
  
  marketOrders <- marketOrders + pos
  #cashFlow <<- cashFlow + currentCashFlow
  #PnL <<- PnLNew
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

#initCFStore  <- function(newRowList,series) {
#  CFStore <- matrix(0,nrow=maxRows,ncol=length(series))
#  return(CFStore)
#}


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

#updateCFStore <- function(CFStore, newRowList, series, iter) {
#  for (i in 1:length(series))
#    CFStore[iter,i] <- currentCashFlow[params1$series[i]]
#  return(CFStore)
#}

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
  #store$CF <- updateCFStore(store$CF,newRowList,series,store$iter)
  return(store)
}