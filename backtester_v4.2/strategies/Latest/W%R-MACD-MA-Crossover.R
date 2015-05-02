#Crossover+limitOrder
#NO Stoploss;target profit;ProfitPos

#params <- list(lookbackWRL=10,lookbackS=5,lookbackL=10,threshold=30,lossLimits=30, profitTarget=30,series=2)#W%R & macd 

maxRows <- 2200
#cashFlow <- rep(0,10)
#PnL <- rep(0,10)
#PnLCurrent <- matrix(0,nrow=maxRows,ncol=10)
#POS <- rep(1,10)
#countOS <- rep(0,10)
#countOB <- rep(0,10)
#count <- rep(0,10)


getOrders <- function(store, newRowList, currentPos, params) {
  #cat("DAY",store$iter,":","\n")
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) {
    #checkParams(params)
    #cashFlow <- rep(0,10)
    #PnL <- rep(0,10)
    store <- initStore(newRowList,params$series)
  }
  else 
    store <- updateStore(store, newRowList,params$series)
  
  #a <<- store$cl
  
  positionSizes <- rep(1,length(newRowList))
  pos <- allzero
  #PnLOld <- allzero
  #PnLNew <- allzero
  #currentCashFlow <- allzero
  #currentWorth <- allzero
  marketOrders <- allzero
  #marketOrders <- ifelse(PnL < -params$lossLimits*POS, -currentPos, 0)
  #marketOrders <- ifelse(PnL > params$profitTarget*POS, -currentPos, 0)
  
  #marketOrders <- ifelse(PnL > params$profitTarget, -currentPos, 0)
  #marketOrders <- sum(marketOrdersL,marketOrdersP)/2
  #print(POS)
  #print(params$lossLimits*POS)
  #print(params$profitTarget*POS)
  #posPnL <- rep(1,10)
  maxLogRet <- allzero
  limitOrders1 <- allzero
  limitPrices1 <- allzero
  limitOrders2 <- allzero
  limitPrices2 <- allzero
  
  lookback <- params$lookbackL + round((params$lookbackL+params$lookbackS)/2)
  
  #cat("cashflow: ",cashFlow,"\n")
  
  if (store$iter >= max(params$lookbackWRL,params$lookbackL,params$lookbackS,lookback)) {
    startIndex <- store$iter - lookback
    startIndexWRL <- store$iter - params$lookbackWRL
    startIndexS <- store$iter - params$lookbackS
    startIndexL <- store$iter - params$lookbackL
    
    for (i in 1:length(params$series)) {
      #marketOrders[i] <- ifelse(PnL[i] < -params$lossLimits || PnL[i] > params$profitTarget, -currentPos[i], 0)
      
      #posPnL[i] <- ifelse(PnLCurrent[store$iter,i] > 0,PnLCurrent[store$iter,i],1)
      #posPnL[i] <- ifelse(posPnL[i]*0.01 > 1,posPnL[i]*0.1,1) #scaling down
      
      #openDiffs <- diff(store$op)
      #absOpenDiffs <- as.matrix(abs(openDiffs))
      #absOpenDiffs <- absOpenDiffs[1:store$iter,]
      #avgAbsDiffs <- colMeans(absOpenDiffs)
      #largestAvgAbsDiff <- max(avgAbsDiffs)
      #positionSizes[i] <- round(largestAvgAbsDiff/avgAbsDiffs[i])
      #positionSizes[i] <- round(positionSizes[i]*posPnL[i])
      #print(store$cl)
      cl <- newRowList[[params$series[i]]]$Close
      highest <- max(store$hi[startIndexWRL:store$iter,i])
      lowest <- min(store$lo[startIndexWRL:store$iter,i])
      R <- (highest - cl)/(highest - lowest)*(-100)
      #cat("W%R: ",R,"\n")
      
      macdData <- MACD(store$cl[startIndex:store$iter,i],
                       nSlow=params$lookbackL,nFast=params$lookbackS,nSig=round((params$lookbackL+params$lookbackS)/2))
      macd <- macdData[,1]
      signal <- macdData[,2]
      
      MAclS <- SMA(store$cl[(startIndexS-1):store$iter,i],n=params$lookbackS)
      MAclL <- SMA(store$cl[startIndexL:store$iter,i],n=params$lookbackL)
      
      #Take a positin when signel line cross(as soon as the trend acceleration appears)
      #if (params$nWait == 0){
      #in a oversold/overbought price level
      if (R < -50 - params$threshold){
        #countOS[i] <<- countOS[i] + 1
        #when there's a bullish crossover
        if (last(MAclS) > last(MAclL) && last(MAclS,n=2)[-2] < last(MAclL,n=2)[-2] 
            || last(macd) > last(signal) && last(macd,n=2)[-2] < last(signal,n=2)[-2]){
          pos[params$series[i]] <- positionSizes[params$series[i]] # long
          #pos[params$series[i]] <- 0.01
          #currentCashFlow[params$series[i]] <- -cl*positionSizes[i]
          #cat("Day",store$iter," Series: ",params$series[i],"\n")
          #cat("cl: ",cl,"\n")
          #cat("positionSize: ",positionSizes[i],"\n")
          #cat("pos: ",pos[params$series[i]],"\n")
          #cat("currentCashFlow: ",currentCashFlow,"\n")
        }         
      }
      #When in overbought price level
      if (R > -50 + params$threshold){
        #countOB[i] <<- countOB[i] + 1
        #when there's a bearish crossover
        if (last(MAclS) < last(MAclL) && last(MAclS,n=2)[-2] > last(MAclL,n=2)[-2] 
            || last(macd) < last(signal) && last(macd,n=2[-2]) > last(signal,n=2)[-2]){
          pos[params$series[i]] <- -positionSizes[params$series[i]]  # short
          #pos[params$series[i]] <- -0.01
          #currentCashFlow[params$series[i]] <- cl*positionSizes[i]
          #cat("Day",store$iter," Series: ",params$series[i],"\n")
          #cat("cl: ",cl,"\n")
          #cat("positionSize: ",- positionSizes[i],"\n")
          #cat("pos: ",pos[params$series[i]],"\n")
          #cat("currentCashFlow: ",currentCashFlow,"\n")
        }
      }
      
      startIndexLimit <- store$iter - params$lookbackWRL
      highest <- max(store$hi[startIndexLimit:store$iter,i])
      lowest <- min(store$lo[startIndexLimit:store$iter,i])
      maxLogRet[params$series[i]] <- max(abs(diff(log(store$cl[startIndexLimit:store$iter,i]))))
      #print(maxLogRet)
      spread <- allzero
      
      spread[params$series[i]] <- maxLogRet[params$series[i]] *10000* (highest - lowest)
      
      b <<- spread
      
      limitOrders1[params$series[i]]  <- positionSizes[params$series[i]] # BUY LIMIT ORDERS
      limitPrices1[params$series[i]]  <- newRowList[[params$series[i]]]$Close - spread[params$series[i]]/2
      
      limitOrders2[params$series[i]]  <- -positionSizes[params$series[i]] # SELL LIMIT ORDERS
      limitPrices2[params$series[i]]  <- newRowList[[params$series[i]]]$Close + spread[params$series[i]]/2
      #}
      
      #currentWorth[params$series[i]] <- ifelse(store$iter == max(params$lookbackR,lookback),
      #cl*pos[params$series[i]],
      #cl*currentPos[params$series[i]]) 
      #currentWorth[params$series[i]] <- cl*currentPos[params$series[i]]
      #PnLOld[params$series[i]] <- PnL[params$series[i]] #Yesterday's profit/loss
      #PnLNew[params$series[i]] <- currentWorth[params$series[i]] +  cashFlow[params$series[i]]
      #PnLCurrent[store$iter,params$series[i]] <<- PnLNew[params$series[i]] - PnLOld[params$series[i]]
      
      #if (marketOrders[i] != 0){
      #count[i] <<- count[i] + 1
      #}
      
    }
  }
  
  #cat("marketOrders before: ",marketOrders,"\n")
  
  marketOrders <- marketOrders + pos
  #cashFlow <<- cashFlow + currentCashFlow
  #PnL <<- PnLNew
  #cat("PnL: ",PnL,"\n")
  #POS <<- positionSizes
  
  #cat("marketOrders: ",marketOrders,"\n")
  #cat("currentPos: ",currentPos,"\n")
  #cat("Oversold times: ",countOS,"\n")
  #cat("Overbought times: ",countOB,"\n")
  #cat("currentCashFlow: ",currentCashFlow,"\n")
  #cat("current worth: ",currentWorth,"\n")
  #cat("PnL: ",PnL,"\n")
  #cat("PnL Current: ",PnLCurrent[store$iter,],"\n")
  #cat("count: ",count,"\n")
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,
              limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,
              limitPrices2=limitPrices2))
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