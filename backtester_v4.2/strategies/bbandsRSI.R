# This strategy only trades on certain series according to params$series.
# The strategy will be long (short) whenever 
# the close is below (above) lower (upper) Bollinger Band of the close.

maxRows <- 200 # used to initialize a matrix to store closing prices

getOrders <- function(store, newRowList, currentPos, params) {

    allzero  <- rep(0,length(newRowList)) # used for initializing vectors

    if (is.null(store)) store <- initStore(newRowList,params$series)
    store <- updateStore(store, newRowList, params$series)
	
    marketOrders <- -currentPos; pos <- allzero

    if (store$iter > params$lookback) {
       startIndex <-  store$iter - params$lookback
       for (i in 1:length(params$series)) {
           cl <- newRowList[[params$series[i]]]$Close
           #print(store$cl[startIndex:store$iter,i])
           #print(RSI(store$cl[startIndex:store$iter,i],n=params$lookback))
           
           rsi <- last(RSI(store$cl[startIndex:store$iter,i],n=params$lookback)) 
         
           if (rsi > (50 + params$threshold))
             pos[params$series[i]] <- -params$posSizes[params$series[i]]
           if (rsi < (50 - params$threshold))
             pos[params$series[i]] <- params$posSizes[params$series[i]]
           
       }
    }
    marketOrders <- marketOrders + pos

    return(list(store=store,marketOrders=marketOrders,
	                    limitOrders1=allzero,limitPrices1=allzero,
	                    limitOrders2=allzero,limitPrices2=allzero))
}
initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  return(store)
}
