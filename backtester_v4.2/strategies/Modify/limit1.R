maxRows <- 2200

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) {
    #checkParams(params)
    cashFlow <- rep(0,10)
    PnL <- rep(0,10)
    store <- initStore(newRowList,params$series)
  }
  else 
    store <- updateStore(store, newRowList,params$series)
  
  posSizes <- rep(1,length(params$series))
  marketOrders <- allzero
  maxLogRet <- rep(0,length(params$series))
  spread <- rep(0,length(params$series))
  avgAbsDiffs <- rep(0,length(params$series))

  limitOrders1 <- allzero
  limitPrices1 <- allzero
  limitOrders2 <- allzero
  limitPrices2 <- allzero
  
  if (store$iter >= max(params$lookbackLimit)) {
    openDiffs <- diff(store$op)
    absOpenDiffs <- as.matrix(abs(openDiffs))
    absOpenDiffs <- absOpenDiffs[1:store$iter,]
    avgAbsDiffs <- colMeans(absOpenDiffs)
    largestAvgAbsDiff <- max(avgAbsDiffs)

    for (i in 1:length(params$series)) {
      #op <- newRowList[[params$series[i]]]$Open
      #largestOpen <- max(store$op)
      #posSizes[i] <- round(largestOpen/op)
      posSizes[i] <- round(largestAvgAbsDiff/avgAbsDiffs[i])
      #if (store$iter >= params$lookbackLimit[i]){
        startIndexLimit <- store$iter - params$lookbackLimit[i]
        highest <- max(store$hi[startIndexLimit:store$iter,i])
        lowest <- min(store$lo[startIndexLimit:store$iter,i])
        #print(store$cl)
        maxLogRet[i] <- max(abs(diff(log(store$cl[startIndexLimit:store$iter,i]))))
        #print(maxLogRet)
        spread[i] <- maxLogRet[i]* (highest - lowest)
        limitOrders1[params$series[i]]  <- posSizes[i]# BUY LIMIT ORDERS
        #print(positionSizes)
        limitPrices1[params$series[i]]  <- newRowList[[params$series[i]]]$Close - spread[i]/2
        
        limitOrders2[params$series[i]]  <- -posSizes[i] # SELL LIMIT ORDERS
        #print(positionSizes)
        limitPrices2[params$series[i]]  <- newRowList[[params$series[i]]]$Close + spread[i]/2
      #}
    }

  }
  
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
#    CFStore[iter,i] <- currentCashFlow[params$series[i]]
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