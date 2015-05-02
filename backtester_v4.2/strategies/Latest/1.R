#Crossover+Stoploss+target profit+ProfitPos
maxRows <- 2200
cashFlow <- rep(0,10)
CPnL <- rep(0,10)
PnLCurrent <- matrix(0,nrow=maxRows,ncol=10)
#POS <- rep(1,10)
#countOS <- rep(0,10)
#countOB <- rep(0,10)
#count <- rep(0,10)


getOrders <- function(store, newRowList, currentPos, params11) {
  cat("DAY",store$iter,":","\n")
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) {
    #checkparams11(params11)
    #cashFlow <- rep(0,10)
    #CPnL <- rep(0,10)
    store <- initStore(newRowList,params1$series)
  }
  else 
    store <- updateStore(store, newRowList,params1$series)
  
  #cashFlow <- rep(0,10)
  #CPnL <- rep(0,10)
  positionSizes <- rep(1,length(newRowList))
  pos <- allzero
  PnLOld <- allzero
  PnLNew <- allzero
  currentCashFlow1 <- allzero
  currentCashFlow2 <- allzero
  currentWorth <- allzero
  marketOrders <- allzero
  posPnL <- rep(1,length(newRowList))
  maxLogRet <- allzero
  limitOrders1 <- allzero
  limitPrices1 <- allzero
  limitOrders2 <- allzero
  limitPrices2 <- allzero
  
  lookback <- params1$lookbackL + round((params1$lookbackL+params1$lookbackS)/2)
  
  #cat("cashflow: ",cashFlow,"\n")
  
  if (store$iter >= max(params1$lookbackWRL,params1$lookbackL,params1$lookbackS,lookback)) {
    
    if(store$iter == max(params1$lookbackWRL,params1$lookbackL,params1$lookbackS,lookback)){
      cashFlow <- allzero
      CPnL <- allzero
    }
    
    startIndex <- store$iter - lookback
    startIndexWRL <- store$iter - params1$lookbackWRL
    startIndexS <- store$iter - params1$lookbackS
    startIndexL <- store$iter - params1$lookbackL
    
    for (i in 1:length(params1$series)) {
      cl <- newRowList[[params1$series[i]]]$Close
      #print(CPnL)
      if(CPnL[params1$series[i]] < -params1$lossLimits || CPnL[params1$series[i]] > params1$profitTarget) {
        marketOrders[params1$series[i]] <- -currentPos[params1$series[i]]
        currentCashFlow1[params1$series[i]] <- currentPos[params1$series[i]]*cl
        #cashFlow[params1$series[i]] <- cashFlow[params1$series[i]] + currentCashFlow[params1$series[i]]
      }
      else
        marketOrders[params1$series[i]] <- 0
      #marketOrders[i] <- ifelse(CPnL[i] < -params1$lossLimits || CPnL[i] > params1$profitTarget, -currentPos[i], 0)
      
      posPnL[params1$series[i]] <- ifelse(PnLCurrent[store$iter,i] > 0,PnLCurrent[store$iter,i],1)
      #posPnL[i] <- ifelse(posPnL[i]*0.01 > 1,posPnL[i]*0.1,1) #scaling down
      
      openDiffs <- diff(store$op)
      absOpenDiffs <- as.matrix(abs(openDiffs))
      absOpenDiffs <- absOpenDiffs[1:store$iter,]
      avgAbsDiffs <- colMeans(absOpenDiffs)
      largestAvgAbsDiff <- max(avgAbsDiffs)
      positionSizes[i] <- round(largestAvgAbsDiff/avgAbsDiffs[i])
      positionSizes[params1$series[i]] <- round(positionSizes[params1$series[i]]*posPnL[params1$series[i]])
      
      highest <- max(store$hi[startIndexWRL:store$iter,i])
      lowest <- min(store$lo[startIndexWRL:store$iter,i])
      R <- (highest - cl)/(highest - lowest)*(-100)
      #cat("W%R: ",R,"\n")
      
      macdData <- MACD(store$cl[startIndex:store$iter,i],
                       nSlow=params1$lookbackL,nFast=params1$lookbackS,nSig=round((params1$lookbackL+params1$lookbackS)/2))
      macd <- macdData[,1]
      signal <- macdData[,2]
      
      MAclS <- SMA(store$cl[(startIndexS-1):store$iter,i],n=params1$lookbackS)
      MAclL <- SMA(store$cl[startIndexL:store$iter,i],n=params1$lookbackL)
      
      #Take a positin when signel line cross(as soon as the trend acceleration appears)
      #if (params1$nWait == 0){
      #in a oversold/overbought price level
      if (R < -50 - params1$threshold){
        #countOS[i] <<- countOS[i] + 1
        #when there's a bullish crossover
        if (last(MAclS) > last(MAclL) && last(MAclS,n=2)[-2] < last(MAclL,n=2)[-2] 
            || last(macd) > last(signal) && last(macd,n=2)[-2] < last(signal,n=2)[-2]){
          pos[params1$series[i]] <- positionSizes[params1$series[i]] # long
          currentCashFlow2[params1$series[i]] <- -cl*positionSizes[params1$series[i]]
        }         
      }
      #When in overbought price level
      if (R > -50 + params1$threshold){
        #countOB[i] <<- countOB[i] + 1
        #when there's a bearish crossover
        if (last(MAclS) < last(MAclL) && last(MAclS,n=2)[-2] > last(MAclL,n=2)[-2] 
            || last(macd) < last(signal) && last(macd,n=2[-2]) > last(signal,n=2)[-2]){
          pos[params1$series[i]] <- -positionSizes[params1$series[i]]  # short
          currentCashFlow2[params1$series[i]] <- cl*positionSizes[params1$series[i]]
        }
      }
      
      startIndexLimit <- store$iter - params1$lookbackWRL
      highest <- max(store$hi[startIndexLimit:store$iter,i])
      lowest <- min(store$lo[startIndexLimit:store$iter,i])
      maxLogRet[params1$series[i]] <- max(abs(diff(log(store$cl[startIndexLimit:store$iter,i]))))
      #print(maxLogRet)
      spread <- allzero
      
      spread[params1$series[i]] <- maxLogRet[params1$series[i]] *10000* (highest - lowest)
      
      
      limitOrders1[params1$series[i]]  <- positionSizes[params1$series[i]] # BUY LIMIT ORDERS
      limitPrices1[params1$series[i]]  <- newRowList[[params1$series[i]]]$Close - spread[params1$series[i]]/2
      
      limitOrders2[params1$series[i]]  <- -positionSizes[params1$series[i]] # SELL LIMIT ORDERS
      limitPrices2[params1$series[i]]  <- newRowList[[params1$series[i]]]$Close + spread[params1$series[i]]/2
      #}
      
      #currentWorth[params1$series[i]] <- ifelse(store$iter == max(params1$lookbackR,lookback),
      #cl*pos[params1$series[i]],
      #cl*currentPos[params1$series[i]]) 
      currentWorth[params1$series[i]] <- cl*currentPos[params1$series[i]]
      PnLOld[params1$series[i]] <- CPnL[params1$series[i]] #Yesterday's profit/loss
      PnLNew[params1$series[i]] <- currentWorth[params1$series[i]] +  cashFlow[params1$series[i]]
      PnLCurrent[store$iter,params1$series[i]] <<- PnLNew[params1$series[i]] - PnLOld[params1$series[i]]
      
      #if (marketOrders[i] != 0){
      #count[i] <<- count[i] + 1
      #}
      
    }
  }
  
  cat("marketOrders before: ",marketOrders,"\n")
  
  marketOrders <- marketOrders + pos
  cashFlow <<- cashFlow + currentCashFlow1 + currentCashFlow2
  CPnL <<- PnLNew
  #cat("PnL: ",CPnL,"\n")
  #POS <<- positionSizes
  
  cat("marketOrders: ",marketOrders,"\n")
  cat("currentPos: ",currentPos,"\n")
  #cat("Oversold times: ",countOS,"\n")
  #cat("Overbought times: ",countOB,"\n")
  #cat("currentCashFlow: ",currentCashFlow,"\n")
  cat("current worth: ",currentWorth,"\n")
  cat("PnL: ",CPnL,"\n")
  cat("PnL Current: ",PnLCurrent[store$iter,],"\n")
  #cat("count: ",count,"\n")
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,
              #limitOrders1=allzero,
              limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,
              #limitOrders2=allzero,
              limitPrices2=limitPrices2
              #cashFlow=cashFlow,
              #CPnL=CPnL
  ))
}

checkparams1 <- function(params1) { # make sure params1 are correct
  if (!"nFast" %in% names(params1))
    stop("Parameter nFast not defined for strategy MACD")
  if (!"nSlow" %in% names(params1))
    stop("Parameter nSlow not defined for strategy MACD")
  if (!"nSig" %in% names(params1))
    stop("Parameter nSig not defined for strategy MACD")
  if (!"nHold" %in% names(params1))
    stop("Parameter nHold not defined for strategy MACD")
  if (!"series" %in% names(params1))
    stop("Parameter series not defined for strategy MACD")
  
  if (params1$nFast >= params1$nSlow)
    stop("Parameter nFast shoule be smaller than nSlow")
  if (params1$nHold < 0 || params1$nHold > 3)
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

initCashFlow  <- function(series) {
  CFStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(CFStore)
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

#updateCFStore <- function(CFStore, newRowList, series, iter) {
#  for (i in 1:length(series))
#    CFStore[iter,i] <- currentCashFlow[params1$series[i]]
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

initPnL <- function(series) {
  return(list(CashFlow=initCashFlow(series),
              CPnL=initPnL(series),
              CurrentPnL=initCurrentPnL(series)))
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

updatePnL <- function(PnL,series) {
  PnL$cashFlow <- updateCashFlow(PnL$cashFlow,series)
  PnL$CPnL <- updateCashFlow(PnL$CPnL,series)
  PnL$currentPnL <- updateCurrentPnL(PnL$currentPnL,series)
}