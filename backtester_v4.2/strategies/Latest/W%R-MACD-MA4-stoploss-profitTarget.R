#No need to crossover
#stopLoss + targetProfit
#profitPos

maxRows <- 2200
cashFlow <- rep(0,10)
PnL <- rep(0,10)
PnLCurrent <- matrix(0,nrow=maxRows,ncol=10)

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
  
  positionSizes <- allzero
  pos <- allzero
  PnLOld <- allzero
  PnLNew <- allzero
  currentCashFlow <- allzero
  currentWorth <- allzero
  marketOrders <- allzero
  #marketOrders <- ifelse(PnL < -params$lossLimits*positionSizes, -currentPos, 0)
  #marketOrders <- ifelse(PnL > params$profitTarget*positionSizes, -currentPos, 0)
  posPnL <- allzero
  limitOrders1 <- allzero
  limitPrices1 <- allzero
  limitOrders2 <- allzero
  limitPrices2 <- allzero
  
  lookback <- params$nSlow + params$nSig
  
  #cat("cashflow: ",cashFlow,"\n")
  
  if (store$iter >= max(params$lookbackR,lookback)) {
    startIndex <- store$iter - lookback
    startIndexR <- store$iter - params$lookbackR
    startIndexS <- store$iter - params$lookbackS
    startIndexL <- store$iter - params$lookbackL
    
    for (i in 1:length(params$series)) {
      marketOrders[i] <- ifelse(PnL[i] < -params$lossLimits || PnL[i] > params$profitTarget, -currentPos[i], 0)
      
      posPnL[i] <- ifelse(PnLCurrent[store$iter,i] > 0,PnLCurrent[store$iter,i],1)
      #posPnL[i] <- ifelse(posPnL[i]*0.01 > 1,posPnL[i]*0.1,1) #scaling down
      
      openDiffs <- diff(store$op)
      absOpenDiffs <- as.matrix(abs(openDiffs))
      absOpenDiffs <- absOpenDiffs[1:store$iter,]
      avgAbsDiffs <- colMeans(absOpenDiffs)
      largestAvgAbsDiff <- max(avgAbsDiffs)
      positionSizes[i] <- round(largestAvgAbsDiff/avgAbsDiffs[i])
      positionSizes[i] <- round(positionSizes[i]*posPnL[i])
      
      cl <- newRowList[[params$series[i]]]$Close
      highest <- max(store$hi[startIndexR:store$iter,i])
      lowest <- min(store$lo[startIndexR:store$iter,i])
      R <- (highest - cl)/(highest - lowest)*(-100)
      
      macdData <- MACD(store$cl[startIndex:store$iter,i],
                       nSlow=params$nSlow,nFast=params$nFast,nSig=params$nSig)
      macd <- macdData[,1]
      signal <- macdData[,2]
      
      MAclS <- last(SMA(store$cl[startIndexS:store$iter,i], n=params$lookbackS))
      MAclL <- last(SMA(store$cl[startIndexL:store$iter,i], n=params$lookbackL))
      
      #Take a positin when signel line cross(as soon as the trend acceleration appears)
      #if (params$nWait == 0){
        #in a oversold/overbought price level
        if (R < -50 - params$threshold){
          #when there's a bullish crossover
          if (MAclS > MAclL || last(macd) > last(signal)){
            pos[params$series[i]] <- positionSizes[i] # long
            currentCashFlow[params$series[i]] <- -cl*positionSizes[i]
          }         
        }
        #When in overbought price level
        if (R > -50 + params$threshold){
          #when there's a bearish crossover
          if (MAclS < MAclL || last(macd) < last(signal)){
            pos[params$series[i]] <- -positionSizes[i]  # short
            currentCashFlow[params$series[i]] <- cl*positionSizes[i]
          }
        }
      #}
      
      #currentWorth[params$series[i]] <- ifelse(store$iter == max(params$lookbackR,lookback),
      #cl*pos[params$series[i]],
      #cl*currentPos[params$series[i]]) 
      currentWorth[params$series[i]] <- cl*currentPos[params$series[i]]
      PnLOld[params$series[i]] <- PnL[params$series[i]] #Yesterday's profit/loss
      PnLNew[params$series[i]] <- currentWorth[params$series[i]] +  cashFlow[params$series[i]]
      PnLCurrent[store$iter,params$series[i]] <<- PnLNew[params$series[i]] - PnLOld[params$series[i]]
      
    }
  }
  marketOrders <- marketOrders + pos
  cashFlow <<- cashFlow + currentCashFlow
  PnL <<- PnLNew
  #positionSizes <<- positionSizes
  
  #print(store$iter)
  #cat("marketOrders: ",marketOrders,"\n")
  #cat("currentPos: ",currentPos,"\n")
  #cat("currentCashFlow: ",currentCashFlow,"\n")
  
  #cat("current worth: ",currentWorth,"\n")
  #cat("PnL: ",PnL,"\n")
  #cat("PnL Current: ",PnLCurrent[store$iter,],"\n")
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,
              limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,
              limitPrices2=limitPrices2,
              #currentCashFlow,
              cashFlow,
              PnL))
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