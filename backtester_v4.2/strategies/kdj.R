maxRows <- 1100 # used to initialize a matrix to store closing prices
getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- -currentPos; pos <- allzero
  
  if (store$iter > params$lookback) {
    startIndex <-  store$iter - params$lookback
    startIndexA <-  store$iter - params$lookbackA
    
    for (i in 1:length(params$series)) {
      cl <- newRowList[[params$series[i]]]$Close
      hlc <- cbind(store$hi[startIndexA:store$iter,i],
                   store$lo[startIndexA:store$iter,i],
                   store$cl[startIndexA:store$iter,i])
      atr <- ATR(hlc,n=params$lookbackA)
      atr <- last(atr[,"atr"])
      
       stochOSC <- stoch(cbind(store$hi[startIndex:store$iter,i],
       store$lo[startIndex:store$iter,i],store$cl[startIndex:store$iter,i]))
       j <- rep(0,length(stochOSC[,'fastK']))
       k <- last(stochOSC[,'fastK'])
       d <- last(stochOSC[,'fastD'])
       j <- 3*d-2*k
       a<-store$iter - 1
       stochOSC2 <- stoch(cbind(store$hi[startIndex:a,i],
                               store$lo[startIndex:a,i],store$cl[startIndex:a,i]),nFastK = 9, nFastD = 3, nSlowD = 3)
       y_k <- last(stochOSC2[,'fastK'])
       y_d <- last(stochOSC2[,'fastD'])
       y_j <- 3*y_d-2*y_k 
       
       
       if(y_k<y_d & k>d&k>80&d>70){ 
         
         pos[params$series[i]] <- 1
    
       }
       
     
       if(y_k>y_d & k<d&k<20&d<30){
         
         pos[params$series[i]] <- -1
       
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
inithiStore  <- function(newRowList,series) {
  hiStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(hiStore)
}
initloStore  <- function(newRowList,series) {
  loStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(loStore)
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
              hi=inithiStore(newRowList,series),
              lo=initloStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  store$hi <- updatehiStore(store$hi,newRowList,series,store$iter)
  store$lo <- updateloStore(store$lo,newRowList,series,store$iter)
  return(store)
}
