maxRows <- 1100

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) {
    checkParams(params)
    store <- initStore(newRowList,params$series)
  }
  else 
    store <- updateStore(store, newRowList,params$series)
  #print(newRowList) #for debug
  #print(store) #for debug
  
  #marketOrders <- -currentPos #???
  #cat("marketOrders:",marketOrders,"\n") #for debug
  marketOrders <- allzero
  limitOrders1 <- allzero  
  limitPrices1 <- allzero
  limitOrders2 <- allzero
  limitPrices2 <- allzero
  
  if (store$iter > params$lookbackL) {
    
    startIndexS <- store$iter - params$lookbackS
    startIndexL <- store$iter - params$lookbackL
    #cat("startIndex",startIndexS,"\n") #for debug
    #cat("iter:",store$iter,"\n") #for debug
    
    for (i in 1:length(params$series)) {
      #print(newRowList)
      #print(store$cl)
      cl <- newRowList[[params$series[i]]]$Close
      #print(cl)
      
      highest <- max(store$hi[startIndexS:store$iter,i], n=params$lookbackS)
      #print("highest:")
      #print(highest)
      lowest <- min(store$lo[startIndexS:store$iter,i], n=params$lookbackS)
      #print("lowest:")
      #print(lowest)
      R <- (highest - cl)/(highest - lowest)*(-100)
      #print("R:")
      #cat(R,"\n")
      
      MAcl <- last(SMA(store$cl[startIndexL:store$iter,i], n=params$lookbackL))
      #print(MAcl)
      
      #When oversold in a long-term uptrend
      if (R < -50 - params$threshold && cl > MAcl){
        pos[params$series[i]] <- params$posSizes[params$series[i]]  # long
      }
      #When overbought in a long-term downtrend
      if (R > -50 + params$threshold && cl < MAcl){
        pos[params$series[i]] <- -params$posSizes[params$series[i]]  # short
      }
    }
  }
  marketOrders <- marketOrders + pos
  #print(currentPos)
  clearInventory <- ifelse(abs(currentPos) > params$inventoryLimits, -currentPos, 0)
  #print("clearInv:")
  #print(clearInventory)
  marketOrders <- marketOrders + clearInventory
  #print("marketOrders:")
  #print(marketOrders)
  
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
inithiStore  <- function(newRowList,series) {
  hiStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(hiStore)
}
initloStore  <- function(newRowList,series) {
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
updatehiStore <- function(hiStore, newRowList, series, iter) {
  for (i in 1:length(series))
    hiStore[iter,i] <- as.numeric(newRowList[[series[i]]]$High)
  return(hiStore)
}
updateloStore <- function(loStore, newRowList, series, iter) {
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
