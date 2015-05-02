# This strategy only trades on certain series according to params$series.

maxRows <- 1100 # used to initialize a matrix to store closing prices

getOrders <- function(store, newRowList, currentPos, params) {

    allzero  <- rep(0,length(newRowList)) # used for initializing vectors

    if (is.null(store)) store <- initStore(newRowList,params$series)
    store <- updateStore(store, newRowList, params$series)
	
    #marketOrders <- -currentPos; 
    marketOrders<-allzero
    pos <- allzero

    if (store$iter > params$lookback) {
       startIndex <-  store$iter - params$lookback
       for (i in 1:length(params$series)) {
           cl <- newRowList[[params$series[i]]]$Close
           rsi <- last(RSI(store$cl[startIndex:store$iter,i],n=params$lookback)) 
           rsi1 <- last(RSI(store$cl[startIndex:store$iter,i],params$lookback1))
           rsi2 <- last(RSI(store$cl[startIndex:store$iter,i],params$lookback2))
           #short
           #decide the strength of market
           if (rsi > (50 + params$threshold)&&rsi1<rsi2)
           {
             #decide the direction of price
            if(store$cl[store$iter,i]<store$cl[store$iter-1,i])
             pos[params$series[i]] <- -params$posSizes[params$series[i]]
           }
           #long
           #decide the strength of market
           if (rsi < (50 - params$threshold)&&rsi1>rsi2)
             {
             #decide the direction of price 
             if(store$cl[store$iter,i]>store$cl[store$iter-1,i])
               pos[params$series[i]] <- params$posSizes[params$series[i]]
            }
                         
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
