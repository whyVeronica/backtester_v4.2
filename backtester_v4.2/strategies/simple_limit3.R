maxRows <- 550

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) {
    #checkParams(params)
    store <- initStore(newRowList,params$series)
  }
  else 
    store <- updateStore(store, newRowList,params$series)
  
  marketOrders <- ifelse(abs(currentPos) > params$inventoryLimits, -currentPos, 0)
  #marketOrders <- allzero
  positionSizes <- allzero
  positionSizesP <- allzero
  positionSizesR <- allzero
  limitOrders1 <- allzero
  limitPrices1 <- allzero
  limitOrders2 <- allzero
  limitPrices2 <- allzero
  maLogRet <- allzero
  
  
  if (store$iter > max(params$lookbackR,params$lookbackLimit)) {
    startIndexR <- store$iter - params$lookbackR
    
    for (i in 1:length(params$series)) {
      largestOpen <- max(store$op)
      openRets <- as.matrix(diff(log(store$op[1:store$iter,])))
      absOpenRets <- as.matrix(abs(openRets))
      absOpenRets <- absOpenRets
      avgAbsRet <- colMeans(absOpenRets,na.rm=TRUE)
      largestAvgAbsRet <- max(avgAbsRet)
      
      op <- newRowList[[params$series[i]]]$Open
      positionSizesP[i] <- round(largestOpen/op)
      positionSizesR[i] <- round(largestAvgAbsRet/avgAbsRet[i])
      positionSizes[i] <- positionSizesP[i]*positionSizesR[i]
      
      cl <- newRowList[[params$series[i]]]$Close
      
      startIndexLimit <- store$iter - params$lookbackLimit
      highma <- SMA(store$hi[startIndexLimit:store$iter,i],n=params$lookbackLimit)
      lowma <- SMA(store$lo[startIndexLimit:store$iter,i],n=params$lookbackLimit)
      #maLogRet[i] <- mean(abs(diff(log(store$cl[startIndexLimit:store$iter,i]))))
      
      spread <- sapply(1:length(newRowList),function(i)
        params$spreadPercentage * (last(highma) - last(lowma)))
      
      limitOrders1  <- positionSizes # BUY LIMIT ORDERS
      limitPrices1  <- sapply(1:length(newRowList),function(i) 
        newRowList[[i]]$Close - spread[i]/2)
      
      limitOrders2  <- -positionSizes # SELL LIMIT ORDERS
      limitPrices2  <- sapply(1:length(newRowList),function(i) 
        newRowList[[i]]$Close + spread[i]/2)
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