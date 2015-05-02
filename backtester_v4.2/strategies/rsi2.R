maxRows <- 200

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  #checkParams(params)
  marketOrders <- -currentPos #???
  pos <- allzero
  
  if (store$iter > params$lookback) {
    startIndex <-  store$iter - params$lookback
    for (i in 1:length(params$series)) {
      cl <- newRowList[[params$series[i]]]$Close
      startIndex <- store$iter - params$lookback
      rsi <- last(RSI(store$cl[startIndex:store$iter,i],n=params$lookback)) 
      
      if (rsi > (50 + params$threshold))
        pos[params$series[i]] <- -params$posSizes[params$series[i]] # short
      if (rsi < (50 - params$threshold))
        pos[params$series[i]] <- params$posSizes[params$series[i]]  # long
             
      }
    }
  
  marketOrders <- marketOrders + pos
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}

checkParams <- function(params) { # make sure params are correct
  if (!"lookback" %in% names(params))
    stop("Parameter lookback not defined for strategy RSI")
  if (!"threshold" %in% names(params))
    stop("Parameter lookback not defined for strategy RSI")
  if (params$threshold < 0 || params$threshold > 50)
    stop("Parameter lookback is not between 0 and 50")
}

####################################################################################
# All the subsequent functions were designed to simplify and 
# improve the readaility of getNewPos(); 
#
# While not required, this type of function-based approach is advisable 
# and these functions will likely have analogues in your strategies
####################################################################################

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