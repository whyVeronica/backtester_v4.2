maxrows <- 600

getOrders <- function(store, newRowList, currentPos, params) {
  
  if (is.null(store)) {
    store <- initStore(newRowList)
  }
  else 
    store <- updateStore(store, newRowList)
  
  marketOrders <- rep(0,length(newRowList))
  limitOrders1 <- rep(0,length(newRowList))
  limitPrices1 <- rep(0,length(newRowList))
  limitOrders2 <- rep(0,length(newRowList))
  limitPrices2 <- rep(0,length(newRowList))
  
  lookback <- params$nSlow + params$nSig   
  
  if (store$iter > lookback) {
    marketOrders   <- sapply(1:length(newRowList),
                             function(x) Macd(store$cl,x,store$iter))
  }
  
  # exit positions from yesterday
  marketOrders <- marketOrders - currentPos 
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,
              limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,
              limitPrices2=limitPrices2))
}


####################################################################################
# All the subsequent functions were designed to simplify and 
# improve the readaility of getNewPos(); 
#
# While not required, this type of function-based approach is advisable 
# and these functions will likely have analogues in your strategies
####################################################################################

initClStore  <- function(newRowList) {
  clStore <- matrix(0,nrow=maxrows,ncol=length(newRowList))
  clStore <- updateClStore(clStore, newRowList, iter=1)
  return(clStore)
}

updateClStore <- function(clStore, newRowList, iter) {
  for (i in 1:length(newRowList))
    clStore[iter,i] <- as.numeric(newRowList[[i]]$Close)
  return(clStore)
}

initStore <- function(newRowList) {
  return(list(iter=1,cl=initClStore(newRowList)))
}

updateStore <- function(store, newRowList) {
  store$iter   <- store$iter + 1
  store$cl	<- updateClStore(store$cl,newRowList,store$iter) 
  return(store)
}

####################################################################################

Macd <-  function(clStore,column,iter) {
  lookback <- params$nSlow + params$nSig
  startIndex <- iter - lookback - 1
  
  slow <- EMA(clStore[startIndex:iter,column], n=params$nSlow)
  fast <- EMA(clStore[startIndex:iter,column], n=params$nFast)
  macd <- fast - slow
  signal <- EMA(macd, n=params$nSig)
  
  #macdData <- last(MACD(clStore[startIndex:iter,column],
  #nFast=params$nFast,nSlow=params$nSlow,nSig=params$nSig))
  
  #macd <- macdData$macd
  #signal <- macdData$signal
  
  
  #Take a positin when signel line cross(as soon as the trend acceleration appears)
  #if (last(macd) > last(signal) && last(macd,n=2) < last(signal,n=2))
    #return(1) # long
  #if (last(macd) < last(signal) && last(macd,n=2) > last(signal,n=2))
    #return(-1)  # short
  
  #Take a position when the trend hold for one more day
  if (last(macd) > last(signal) && last(macd,n=2) > last(signal,n=2) && last(macd,n=3) < last(signal,n=3))
    return(1) # long
  if (last(macd) < last(signal) && last(macd,n=2) < last(signal,n=2) && last(macd,n=3) > last(signal,n=3))
    return(-1)  # short
  
  return(0)
}
