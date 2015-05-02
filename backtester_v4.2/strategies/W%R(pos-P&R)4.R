maxRows <- 1100

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) {
    #checkParams(params)
    store <- initStore(newRowList,params$series)
  }
  else 
    store <- updateStore(store, newRowList,params$series)
  #print(newRowList) #for debug
  #print(store) #for debug
  
  #marketOrders <- -currentPos #???
  #cat("marketOrders:",marketOrders,"\n") #for debug
  pos <- allzero
  positionSizesP <- allzero
  positionSizesR <- allzero
  positionSizes <- allzero
  #marketOrders <- ifelse(abs(currentPos) > params$inventoryLimits, -currentPos, 0)
  marketOrders <- allzero
  limitOrders1 <- allzero  
  limitPrices1 <- allzero
  limitOrders2 <- allzero
  limitPrices2 <- allzero
  maLogRet <- allzero
  
  
  if (store$iter > params$lookbackL) {
    #cat("iter:",store$iter,"\n") #for debug
    largestOpen <- max(store$op)
    openRets <- as.matrix(diff(log(store$op[1:store$iter,])))
    #print("openRets:")
    #print(openRets)
    absOpenRets <- as.matrix(abs(openRets))
    absOpenRets <- absOpenRets
    #print("absOpenRets:")
    #print(absOpenRets)
    avgAbsRet <- colMeans(absOpenRets,na.rm=TRUE)
    #print(avgAbsRet)
    largestAvgAbsRet <- max(avgAbsRet)
    #cat("largestAvgAbsRet:",largestAvgAbsRet,"\n")
    
    startIndexR <- store$iter - params$lookbackR
    startIndexS <- store$iter - params$lookbackS
    startIndexL <- store$iter - params$lookbackL
    #cat("startIndex",startIndexS,"\n") #for debug
    
    for (i in 1:length(params$series)) {
      cl <- newRowList[[params$series[i]]]$Close
      op <- newRowList[[params$series[i]]]$Open
      positionSizesP[params$series[i]] <- round(largestOpen/op)
      positionSizesR[params$series[i]] <- round(largestAvgAbsRet/avgAbsRet[i])
      positionSizes[params$series[i]] <- positionSizesP[params$series[i]]*positionSizesR[params$series[i]]
      
      highest <- max(store$hi[startIndexR:store$iter,i])
      lowest <- min(store$lo[startIndexR:store$iter,i])
      R <- (highest - cl)/(highest - lowest)*(-100)
      
      MAclS <- last(SMA(store$cl[startIndexS:store$iter,i], n=params$lookbackS))
      MAclL <- last(SMA(store$cl[startIndexL:store$iter,i], n=params$lookbackL))
      
      #When oversold in an uptrend
      if (R < -50 - params$threshold && MAclS > MAclL){
        #pos[params$series[i]] <- params$posSizes[params$series[i]]  # long
        pos[params$series[i]] <- positionSizes[params$series[i]]  # long
      }
      #When overbought in a downtrend
      if (R > -50 + params$threshold && MAclS < MAclL){
        #pos[params$series[i]] <- -params$posSizes[params$series[i]]  # short
        pos[params$series[i]] <- -positionSizes[params$series[i]]  # short
      }
      
      spread <- sapply(1:length(newRowList),function(i)
        params$spreadPercentage * (newRowList[[i]]$High -
                                     newRowList[[i]]$Low))
      
      limitOrders1  <- rep(1,length(newRowList)) # BUY LIMIT ORDERS
      limitPrices1  <- sapply(1:length(newRowList),function(i) 
        newRowList[[i]]$Close - spread[i]/2)
      
      limitOrders2  <- rep(-1,length(newRowList)) # SELL LIMIT ORDERS
      limitPrices2  <- sapply(1:length(newRowList),function(i) 
        newRowList[[i]]$Close + spread[i]/2)
    }
  }
  marketOrders <- marketOrders + pos
  #print(currentPos)
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,
              limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,
              limitPrices2=limitPrices2))
}

checkParams <- function(params) { # make sure params are correct
  if (!"lookbackS" %in% names(params))
    stop("Parameter lookbackS not defined for strategy W%R")
  if (!"lookbackL" %in% names(params))
    stop("Parameter lookbackL not defined for strategy W%R")
  if (!"threshold" %in% names(params))
    stop("Parameter threshold not defined for strategy W%R")
  if (!"inventoryLimits" %in% names(params))
    stop("Parameter inventoryLimits not defined for strategy W%R")
  if (!"series" %in% names(params))
    stop("Parameter series not defined for strategy W%R")
  if (!"posSizes" %in% names(params))
    stop("Parameter posSizes not defined for strategy W%R")
  
  if (params$threshold > 50 || params$threshold < 0)
    stop("Parameter threshold shoule be between 0~50")
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
