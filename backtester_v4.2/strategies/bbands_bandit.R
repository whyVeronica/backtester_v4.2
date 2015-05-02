# This strategy only trades on certain series according to params$series.
# The strategy will be long (short) whenever 
# the close is below (above) lower (upper) Bollinger Band of the close.

maxRows <- 1100 # used to initialize a matrix to store closing prices
liqDay <- 50
getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  
  
  marketOrders <- -currentPos; pos <- allzero
  
  if (store$iter > params$lookback) {
    startIndex <-  store$iter - params$lookback
   
    
    for (i in 1:length(params$series)) {
      
      openDiffs <- diff(store$op)
      #print("openDiffs:")
      #print(openDiffs)
      cat("iter:",store$iter,"\n")
      absOpenDiffs <- as.matrix(abs(openDiffs))
      absOpenDiffs <- absOpenDiffs[1:store$iter,]
      #print("absOpenDiffs:")
      #print(absOpenDiffs)
      avgAbsDiffs <- colMeans(absOpenDiffs)
      #print(avgAbsDiffs)
      largestAvgAbsDiff <- max(avgAbsDiffs)
      params$posSizes[params$series[i]] <- round(largestAvgAbsDiff/avgAbsDiffs[i])
      cl <- newRowList[[params$series[i]]]$Close
      # meanCl <- mean(store$cl[startIndex:store$iter,i])
      rocCalc <- cl - store$cl[store$iter-params$rocCalLength,i]
     
      bbands <- last(BBands(store$cl[startIndex:store$iter,i],
                            n=params$lookback,sd=params$sdParam))
   
    
      #entry 
      if(currentPos[i]==0){
        if (cl > bbands["up"]&rocCalc>0) {
          pos[params$series[i]] <- params$posSizes[params$series[i]]
          liqDay<- 50
          print(liqDay)
        }
        else if (cl < bbands["dn"]&rocCalc<0) {
          pos[params$series[i]] <- -params$posSizes[params$series[i]]
          liqDay<- 50
          print(liqDay)
        }
     }
     
    
     averageLiq <- last(SMA((store$cl[,i]),n=liqDay))
      if(currentPos[i] != 0 & averageLiq > bbands["up"]& cl<averageLiq){
        pos[params$series[i]] <- -params$posSizes[params$series[i]]
        liqDay<-liqDay-1
        liqDay<-max(liqDay,10)
        print(liqDay)
      }
     
     if(currentPos[i] != 0 & averageLiq < bbands["dn"] & cl>averageLiq){
       pos[params$series[i]] <- params$posSizes[params$series[i]]
       liqDay<-liqDay-1
       
       liqDay<-max(liqDay,10)
       print(liqDay)
       
     }
    }
  }
  marketOrders <- marketOrders + pos
  
  return(list(store=store,marketOrders=marketOrders,
              liqDay=liqDay,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}
initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}

initopStore  <- function(newRowList,series) {
  opStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(opStore)
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
updateOpStore <- function(opStore, newRowList, series, iter) {
  for (i in 1:length(series))
    opStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Open)
  return(opStore)
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
              lo=initloStore(newRowList,series),
              op=initopStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  store$hi <- updatehiStore(store$hi,newRowList,series,store$iter)
  store$lo <- updateloStore(store$lo,newRowList,series,store$iter)
  store$op <- updateOpStore(store$lo,newRowList,series,store$iter)
  return(store)
}
