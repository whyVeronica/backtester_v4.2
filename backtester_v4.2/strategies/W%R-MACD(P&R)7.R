maxRows <- 1100

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) {
    #checkParams(params)
    store <- initStore(newRowList,params$series)
  }
  else 
    store <- updateStore(store, newRowList,params$series)

  marketOrders <- allzero
  #marketOrders <- ifelse(abs(currentPos) > params$inventoryLimits, -currentPos, 0)
  positionSizes <- allzero
  positionSizesP <- allzero
  positionSizesR <- allzero
  limitOrders1 <- allzero
  limitPrices1 <- allzero
  limitOrders2 <- allzero
  limitPrices2 <- allzero
  
  lookback <- params$nSlow + params$nSig + params$nWait
  
  if (store$iter > max(params$lookbackR,lookback)) {
    startIndex <- store$iter - lookback
    startIndexR <- store$iter - params$lookbackR
    
    for (i in 1:length(params$series)) {
      largestOpen <- max(store$op)
      openRets <- as.matrix(diff(log(store$op[1:store$iter,])))
      absOpenRets <- as.matrix(abs(openRets))
      #absOpenRets <- absOpenRets
      avgAbsRet <- colMeans(absOpenRets,na.rm=TRUE)
      largestAvgAbsRet <- max(avgAbsRet)

      op <- newRowList[[params$series[i]]]$Open
      positionSizesP[i] <- round(largestOpen/op)
      print("posSizeP:")
      print(positionSizesP[i])
      
      positionSizesR[i] <- round(largestAvgAbsRet/avgAbsRet[i])
      print("posSizeR:")
      print(positionSizesR[i])
      
      positionSizes[i] <- positionSizesP[i]*positionSizesR[i]
      
      cl <- newRowList[[params$series[i]]]$Close
      highest <- max(store$hi[startIndexR:store$iter,i], n=params$lookbackR)
      lowest <- min(store$lo[startIndexR:store$iter,i], n=params$lookbackR)
      R <- (highest - cl)/(highest - lowest)*(-100)
      
      macdData <- MACD(store$cl[startIndex:store$iter,i],
                       nSlow=params$nSlow,nFast=params$nFast,nSig=params$nSig)
      macd <- macdData[,1]
      signal <- macdData[,2]
      
      
      #Take a positin when signel line cross(as soon as the trend acceleration appears)
      if (params$nWait == 0){
        #in a oversold/overbought price level
        if (R < -50 - params$threshold){
          #when there's a bullish crossover
          if (last(macd) > last(signal) && last(macd,n=2)[-2] < last(signal,n=2)[-2])
            marketOrders[params$series[i]] <- positionSizes[i] # long
        }
        #When in overbought price level
        if (R > -50 + params$threshold){
          #when there's a bearish crossover
          if (last(macd) < last(signal) && last(macd,n=2[-2]) > last(signal,n=2)[-2])
            marketOrders[params$series[i]] <- -positionSizes[i]  # short
        }
      }
      
      
      #Take a position after the trend has been held for one more day
      if (params$nWait == 1){
        #When oversold 
        if (R < -50 - params$threshold){
          #when there's a bullish crossover that have held for one day
          if (last(macd) > last(signal) && last(macd,n=2)[-2] > last(signal,n=2)[-2]
              && last(macd,n=3)[-c(2,3)] < last(signal,n=3)[-c(2,3)])
            marketOrders[params$series[i]] <- positionSizes[i] # long
        }
        #When overbought 
        if (R > -50 + params$threshold){
          #when there's a bearish crossover that have held for one day
          if (last(macd) < last(signal) && last(macd,n=2)[-2] < last(signal,n=2)[-2]
              && last(macd,n=3)[-c(2,3)] > last(signal,n=3)[-c(2,3)])
            marketOrders[params$series[i]] <- -positionSizes[i]  # short
        }
      }
      
      #Take a position after the trend has been held for two more days
      #in a oversold/overbought price level
      if (params$nWait == 2){
        #When in oversold price level
        if (R < -50 - params$threshold){
          #when there's a bullish crossover that have held for two days
          if (last(macd) > last(signal) && last(macd,n=2)[-2] > last(signal,n=2)[-2]
              && last(macd,n=3)[-c(2,3)] > last(signal,n=3)[-c(2,3)] 
              && last(macd,n=4)[-c(2:4)] < last(signal,n=4)[-c(2:4)])
            marketOrders[params$series[i]] <- positionSizes[i] # long
        }
        #When in overbought price level
        if (R > -50 + params$threshold){
          #when there's a bearish crossover that have held for two days
          if (last(macd) < last(signal) && last(macd,n=2)[-2] < last(signal,n=2)[-2]
              && last(macd,n=3)[-c(2,3)] < last(signal,n=3)[-c(2,3)] 
              && last(macd,n=4)[-c(2:4)] > last(signal,n=4)[-c(2:4)])
            marketOrders[params$series[i]] <- -positionSizes[i]  # short
        }
      }
      
      #Take a position after the trend has been held for three days
      if (params$nWait == 3){
        #When oversold
        if (R < -50 - params$threshold){
          #when there's a bullish crossover that have held for two days
          if (last(macd) > last(signal) && last(macd,n=2)[-2] > last(signal,n=2)[-2]
              && last(macd,n=3)[-c(2,3)] > last(signal,n=3)[-c(2,3)] 
              && last(macd,n=4)[-c(2:4)] > last(signal,n=4)[-c(2:4)]
              && last(macd,n=5)[-c(2:5)] < last(signal,n=5)[-c(2:5)])
            marketOrders[params$series[i]] <- positionSizes[i] # long
        }
        #When overbought
        if (R > -50 + params$threshold){
          #when there's a bearish crossover that have held for two days
          if (last(macd) < last(signal) && last(macd,n=2)[-2] < last(signal,n=2)[-2]
              && last(macd,n=3)[-c(2,3)] < last(signal,n=3)[-c(2,3)] 
              && last(macd,n=4)[-c(2:4)] < last(signal,n=4)[-c(2:4)] 
              && last(macd,n=5)[-c(2:5)] > last(signal,n=5)[-c(2:5)])
            marketOrders[params$series[i]] <- -positionSizes[i]  # short
        }
      }
      
    }
    #print("positionSizes:")
    #print(positionSizes)
  }
  #marketOrders <- marketOrders + pos
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,
              limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,
              limitPrices2=limitPrices2))
}

checkParams <- function(params) { # make sure params are correct
  if (!"nFast" %in% names(params))
    stop("Parameter nFast not defined for strategy MACD")
  if (!"nSlow" %in% names(params))
    stop("Parameter nSlow not defined for strategy MACD")
  if (!"nSig" %in% names(params))
    stop("Parameter nSig not defined for strategy MACD")
  if (!"nHold" %in% names(params))
    stop("Parameter nHold not defined for strategy MACD")
  if (!"series" %in% names(params))
    stop("Parameter series not defined for strategy MACD")
  
  if (params$nFast >= params$nSlow)
    stop("Parameter nFast shoule be smaller than nSlow")
  if (params$nHold < 0 || params$nHold > 3)
    stop("Parameter nHold can only be 0~3 (Longer holding period has not been coded yet).")
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