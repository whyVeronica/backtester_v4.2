# This strategy only trades on certain series according to params$series.
# The strategy will be long (short) whenever 
# the close is below (above) lower (upper) Bollinger Band of the close.
source('framework/processResults.R');

maxRows <- 2200 # used to initialize a matrix to store closing prices
liqDay <- 50
CPnL1 <- CPnL2 <- matrix(0,nrow=maxRows,ncol=10)
CPnL3 <- CPnL4 <- CPnL5 <- matrix(0,nrow=maxRows,ncol=10)
PnLCurrent1 <- PnLCurrent2 <- matrix(0,nrow=maxRows,ncol=10)
PnLCurrent3 <- PnLCurrent4 <- PnLCurrent5 <- matrix(0,nrow=maxRows,ncol=10)
currentCashFlow <-  currentCashFlow1 <- currentCashFlow2 <-  matrix(0,nrow=maxRows,ncol=10)
currentCashFlow <-  currentCashFlow3 <- currentCashFlow4 <- currentCashFlow5<-matrix(0,nrow=maxRows,ncol=10)
currentCashFlow11 <- currentCashFlow21 <- currentCashFlow31 <- currentCashFlow41 <- currentCashFlow51 <- matrix(0,nrow=maxRows,ncol=10)
cashFlow1 <- cashFlow2 <- matrix(0,nrow=maxRows,ncol=10)
cashFlow3 <- cashFlow4 <- cashFlow5<- matrix(0,nrow=maxRows,ncol=10)
currentPos1 <- currentPos2 <- rep(0,10)
currentPos3 <- currentPos4 <- currentPos5 <- rep(0,10)
maxDrawdown1 <- maxDrawdown2 <- matrix(0,nrow=maxRows,ncol=10)
maxDrawdown3 <- maxDrawdown4 <- maxDrawdown5 <- matrix(0,nrow=maxRows,ncol=10)
initialLongPrice1 <- initialLongPrice2 <- initialLongPrice3 <- initialLongPrice4 <- initialLongPrice5 <- initialLongPrice6 <- initialLongPrice7 <- rep(0,10) 
initialShortPrice1 <- initialShortPrice2 <- initialShortPrice3 <- initialShortPrice4 <- initialShortPrice5 <- initialShortPrice6 <- initialShortPrice7 <- rep(0,10)
longCount1 <- longCount2 <- longCount3 <- longCount4 <- longCount5 <- longCount6 <- longCount7 <- rep(0,10)
shortCount1 <- shortCount2 <- shortCount3 <- shortCount4 <- shortCount5 <- shortCount6 <- shortCount7 <- rep(0,10)

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)

  pos <- pos1 <- pos2 <- pos3 <- pos4 <- pos5 <- allzero
  marketOrders <- marketOrders1 <- marketOrders2 <- allzero
  marketOrders3 <- marketOrders4 <- marketOrders5 <- allzero
  posSizes <- rep(1,length(newRowList))
  #absOpenDiffs <- matrix(0,1299,10)
  pos1 <- pos2 <- pos3 <- pos4<- pos5<-  allzero
  #PnLOld1 <- PnLOld2 <- allzero
  PnLNew <- PnLNew1 <- PnLNew2 <- PnLNew3 <- PnLNew4 <-PnLNew5 <- allzero
  posPnL <- rep(1,length(newRowList))
  currentWorth <- currentWorth1 <- currentWorth2 <-currentWorth3 <- currentWorth4 <- currentWorth5 <- allzero
  MAPnL1 <- MAPnL2 <- allzero
  MAPnL3 <- MAPnL4<- MAPnL5 <- allzero
  lookback1 <- params$lookbackL1 + round((params$lookbackL1+params$lookbackS1)/2)
  maxLookback1 <- max(params$lookbackWRL1,params$lookbackL1,params$lookbackS1,lookback1)
  lookback2 <- params$lookbackL2 + round((params$lookbackL2+params$lookbackS2)/2)
  maxLookback2 <- max(params$lookbackWRL2,params$lookbackL2,params$lookbackS2,lookback2)
  lookbackm <- params$nSlow + params$nSig
  
  if (store$iter > params$lookback) {
    
    
    for (i in 1:length(params$series)) {
      if (i==8){
        cl <- newRowList[[params$series[i]]]$Close
        if (currentPos[params$series[i]] > 0) {
          if (cl >= initialLongPrice1[params$series[i]]*params$targetProfit) {
            marketOrders[params$series[i]] <- -currentPos[params$series[i]]
            #currentCashFlow11[store$iter,params$series[i]] <- currentPos[params$series[i]]*cl
          }
        }
        
        if (currentPos[params$series[i]] < 0) {
          if (cl <= initialShortPrice1[params$series[i]]*params$targetProfit) {
            marketOrders[params$series[i]] <- -currentPos[params$series[i]]
            #currentCashFlow11[store$iter,params$series[i]] <- -currentPos[params$series[i]]*cl
          }
        }
        
        startIndex <-  store$iter - params$lookback
        openDiffs <- diff(store$op)
        absOpenDiffs <- as.matrix(abs(openDiffs))
        absOpenDiffs <- absOpenDiffs[1:store$iter,]
        #print(absOpenDiffs[1:store$iter,])
        avgAbsDiffs <- colMeans(absOpenDiffs)
        largestAvgAbsDiff <- max(avgAbsDiffs)
        posSizes[params$series[i]] <- round(largestAvgAbsDiff/avgAbsDiffs[params$series[i]])
        
        marketOrders[i] <- -currentPos[i]
        
        
        rocCalc <- cl - store$cl[store$iter-params$rocCalLength,i]
        
        bbands <- last(BBands(store$cl[startIndex:store$iter,i],
                              n=params$lookback,sd=params$sdParam))
        
        
        #entry 
        averageLiq <- last(SMA((store$cl[,i]),n=liqDay))
        if(currentPos[i]==0){
          if (cl > bbands["up"]&rocCalc>0) {
            pos[params$series[i]] <- posSizes[params$series[i]]
            if (initialLongPrice1[params$series[i]] != 0) {
              longCount1[params$series[i]] <<- longCount1[params$series[i]] + 1
              initialLongPrice1[params$series[i]] <<- (initialLongPrice1[params$series[i]]*longCount1[params$series[i]] + cl)/(longCount1[params$series[i]] + 1)
            }
            else {
              initialLongPrice1[params$series[i]] <<- cl
            }
            
            liqDay<- 50   
            # print(1)
          }
          else if (cl < bbands["dn"]&rocCalc<0) {
            pos[params$series[i]] <- -posSizes[params$series[i]]
            
            if (initialShortPrice1[params$series[i]] != 0) {
              shortCount1[params$series[i]] <<- shortCount1[params$series[i]] + 1
              initialShortPrice1[params$series[i]] <<- (initialShortPrice1[params$series[i]]*shortCount1[params$series[i]] + cl)/(shortCount1[params$series[i]] + 1)
            }
            else {
              initialShortPrice1[params$series[i]] <<- cl
            }
            
            liqDay<- 50
            # print(2)
          }
        }
        
        #short
        averageLiq <- last(SMA((store$cl[,i]),n=liqDay))
        if(currentPos[i]!=0 && cl<averageLiq){
          pos[params$series[i]] <- -posSizes[params$series[i]]
          
          if (initialShortPrice1[params$series[i]] != 0) {
            shortCount1[params$series[i]] <<- shortCount1[params$series[i]] + 1
            initialShortPrice1[params$series[i]] <<- (initialShortPrice1[params$series[i]]*shortCount1[params$series[i]] + cl)/(shortCount1[params$series[i]] + 1)
          }
          else {
            initialShortPrice1[params$series[i]] <<- cl
          }
          
          liqDay<-liqDay-1
          liqDay<-max(liqDay,10) 
          # print(3)
        } 
        #long
        if(currentPos[i]!=0 && cl>averageLiq){
          pos[params$series[i]] <- posSizes[params$series[i]]
          if (initialLongPrice1[params$series[i]] != 0) {
            longCount1[params$series[i]] <<- longCount1[params$series[i]] + 1
            initialLongPrice1[params$series[i]] <<- (initialLongPrice1[params$series[i]]*longCount1[params$series[i]] + cl)/(longCount1[params$series[i]] + 1)
          }
          else {
            initialLongPrice1[params$series[i]] <<- cl
          }
          liqDay<-liqDay-1
          liqDay<-max(liqDay,10) 
          # print(4)
        }
      }
    }
  }
    
    
  if (store$iter > max(params$lookback_macd,lookbackm)) {  
    
    
    for (i in 1:length(params$series)) {
      
      if(i==4) {
        cl <- newRowList[[params$series[i]]]$Close
        
        if (currentPos[params$series[i]] > 0) {
          if (cl >= initialLongPrice2[params$series[i]]*params$targetProfit) {
            marketOrders[params$series[i]] <- -currentPos[params$series[i]]
            #currentCashFlow11[store$iter,params$series[i]] <- currentPos[params$series[i]]*cl
          }
        }
        
        if (currentPos[params$series[i]] < 0) {
          if (cl <= initialShortPrice2[params$series[i]]*params$targetProfit) {
            marketOrders[params$series[i]] <- -currentPos[params$series[i]]
            #currentCashFlow11[store$iter,params$series[i]] <- -currentPos[params$series[i]]*cl
          }
        }
        
        startIndex <-  store$iter - params$lookback
        openDiffs <- diff(store$op)
        absOpenDiffs <- as.matrix(abs(openDiffs))
        absOpenDiffs <- absOpenDiffs[1:store$iter,]
        #print(absOpenDiffs[1:store$iter,])
        avgAbsDiffs <- colMeans(absOpenDiffs)
        largestAvgAbsDiff <- max(avgAbsDiffs)
        posSizes[params$series[i]] <- round(largestAvgAbsDiff/avgAbsDiffs[params$series[i]])
        
        startIndex_macd <-  store$iter - params$lookback_macd
        startIndex1_macd <-  store$iter - params$lookback1_macd
        startIndex2_macd <-  store$iter - params$lookback2_macd
        startIndexm_macd <-  store$iter - lookbackm
        marketOrders[i] <- 0
        
        
        op <- newRowList[[params$series[i]]]$Open         
        rsi <- last(RSI(store$cl[startIndex_macd:store$iter,i],n=params$lookback_macd)) 
        rsi1 <- last(RSI(store$cl[startIndex1_macd:store$iter,i],params$lookback1_macd))
        rsi2 <- last(RSI(store$cl[startIndex2_macd:store$iter,i],params$lookback2_macd))
        
        macdData <- MACD(store$cl[startIndexm_macd:store$iter,i],nSlow=params$nSlow,nFast=params$nFast,nSig=params$nSig)
        #print(macdData)
        macd <- macdData[,1]
        signal <- macdData[,2]
        #short
        #decide the strength of market
        if (rsi > (50 + params$threshold)&&rsi1<rsi2) {
          #decide the direction of price
          if(store$cl[store$iter,i]<store$cl[store$iter-1,i]
             && last(macd) < last(signal) 
             && last(macd,n=2[-2]) > last(signal,n=2)[-2]){
            pos[params$series[i]] <- -posSizes[params$series[i]]
            
            if (initialShortPrice2[params$series[i]] != 0) {
              shortCount2[params$series[i]] <<- shortCount2[params$series[i]] + 1
              initialShortPrice2[params$series[i]] <<- (initialShortPrice2[params$series[i]]*shortCount2[params$series[i]] + cl)/(shortCount2[params$series[i]] + 1)
            }
            else {
              initialShortPrice2[params$series[i]] <<- cl
            }
            
          }
        }
        
        #long1
        if (rsi>50 && rsi < (50 + params$threshold)) {
          #decide the direction of price
          if(store$cl[store$iter,i]>store$cl[store$iter-1,i]
             &&last(macd) > last(signal) 
             && last(macd,n=2)[-2] < last(signal,n=2)[-2]){
            pos[params$series[i]] <- posSizes[params$series[i]]
            if (initialLongPrice2[params$series[i]] != 0) {
              longCount2[params$series[i]] <<- longCount2[params$series[i]] + 1
              initialLongPrice2[params$series[i]] <<- (initialLongPrice2[params$series[i]]*longCount2[params$series[i]] + cl)/(longCount2[params$series[i]] + 1)
            }
            else {
              initialLongPrice2[params$series[i]] <<- cl
            }
          }   
        }
        
        #long2
        #decide the strength of market
        if (rsi < (50 - params$threshold)&&rsi1>rsi2){
          #decide the direction of price 
          if(store$cl[store$iter,i]>store$cl[store$iter-1,i]
             &&last(macd) > last(signal) 
             && last(macd,n=2)[-2] < last(signal,n=2)[-2]) {
            pos[params$series[i]] <-  posSizes[params$series[i]]
            if (initialLongPrice2[params$series[i]] != 0) {
              longCount2[params$series[i]] <<- longCount2[params$series[i]] + 1
              initialLongPrice2[params$series[i]] <<- (initialLongPrice2[params$series[i]]*longCount2[params$series[i]] + cl)/(longCount2[params$series[i]] + 1)
            }
            else {
              initialLongPrice2[params$series[i]] <<- cl
            }
          }  
        }
      }
      }
    }
    
    
    
  if (store$iter > max(maxLookback1,maxLookback2)) {  
    for (i in 1:length(params$series)) {
      if(i!=4&i!=8&i!=3&i!=6)  {
        cl <- newRowList[[params$series[i]]]$Close
        
        if (currentPos1[params$series[i]] > 0) {
          if (cl >= initialLongPrice3[params$series[i]]*params$targetProfit) {
            marketOrders1[params$series[i]] <- -currentPos1[params$series[i]]
            currentCashFlow11[store$iter,params$series[i]] <- currentPos[params$series[i]]*cl
          }
        }
        
        if (currentPos1[params$series[i]] < 0) {
          if (cl <= initialShortPrice3[params$series[i]]*params$targetProfit) {
            marketOrders1[params$series[i]] <- -currentPos1[params$series[i]]
            currentCashFlow11[store$iter,params$series[i]] <- -currentPos[params$series[i]]*cl
          }
        }
        ##########################
        if (currentPos2[params$series[i]] > 0) {
          if (cl >= initialLongPrice4[params$series[i]]*params$targetProfit) {
            marketOrders2[params$series[i]] <- -currentPos2[params$series[i]]
            currentCashFlow21[store$iter,params$series[i]] <- currentPos[params$series[i]]*cl
          }
        }
        
        if (currentPos2[params$series[i]] < 0) {
          if (cl <= initialShortPrice4[params$series[i]]*params$targetProfit) {
            marketOrders2[params$series[i]] <- -currentPos2[params$series[i]]
            currentCashFlow21[store$iter,params$series[i]] <- -currentPos[params$series[i]]*cl
          }
        }
      
        
        
        startIndex <-  store$iter - params$lookback
        openDiffs <- diff(store$op)
        absOpenDiffs <- as.matrix(abs(openDiffs))
        absOpenDiffs <- absOpenDiffs[1:store$iter,]
        #print(absOpenDiffs[1:store$iter,])
        avgAbsDiffs <- colMeans(absOpenDiffs)
        largestAvgAbsDiff <- max(avgAbsDiffs)
        posSizes[params$series[i]] <- round(largestAvgAbsDiff/avgAbsDiffs[params$series[i]])
        
        marketOrders[i] <- marketOrders1[i] <- marketOrders2[i] <- 0
        lookback1 <- params$lookbackL1[i] + round((params$lookbackL1[i]+params$lookbackS1[i])/2)
        
        lookback2 <- params$lookbackL2[i] + round((params$lookbackL2[i]+params$lookbackS2[i])/2)
        
        startIndex1 <- store$iter - lookback1
        #print(startIndex1)
        startIndexWRL1 <- store$iter - params$lookbackWRL1[i]
        startIndexS1 <- store$iter - params$lookbackS1[i]
        startIndexL1 <- store$iter - params$lookbackL1[i]
        
        startIndex2 <- store$iter - lookback2
        startIndexWRL2 <- store$iter - params$lookbackWRL2[i]
        startIndexS2 <- store$iter - params$lookbackS2[i]
        startIndexL2 <- store$iter - params$lookbackL2[i]
        #posPnL[i] <- ifelse(PnLCurrent[store$iter,i] > 0,PnLCurrent[store$iter,i],1)
        #posPnL[i] <- ifelse(posPnL[i]*0.01 > 1,posPnL[i]*0.1,1) #scaling down
        
        #print(store$cl)
        
        #currentWorth[params$series[i]] <- cl*currentPos[params$series[i]] 
        highest1 <- max(store$hi[startIndexWRL1:store$iter,i])
        lowest1 <- min(store$lo[startIndexWRL1:store$iter,i])
        R1 <- (highest1 - cl)/(highest1 - lowest1)*(-100)
        #cat("W%R: ",R,"\n")
        
        macdData1 <- MACD(store$cl[startIndex1:store$iter,i],
                          nSlow=params$lookbackL1[i],nFast=params$lookbackS1[i],nSig=round((params$lookbackL1[i]+params$lookbackS1[i])/2))
        macd1 <- macdData1[,1]
        signal1 <- macdData1[,2]
        
        MAclS1 <- SMA(store$cl[(startIndexS1-1):store$iter,i],n=params$lookbackS1[i])
        MAclL1 <- SMA(store$cl[startIndexL1:store$iter,i],n=params$lookbackL1[i])
        
        #Take a positin when signel line cross(as soon as the trend acceleration appears)
        #if (params$nWait == 0){
        #in a oversold/overbought price level
        if (R1 < -50 - params$threshold1_wr[i]){
          #countOS[i] <<- countOS[i] + 1
          #when there's a bullish crossover
          if (last(MAclS1) > last(MAclL1) && last(MAclS1,n=2)[-2] < last(MAclL1,n=2)[-2] 
              || last(macd1) > last(signal1) && last(macd1,n=2)[-2] < last(signal1,n=2)[-2]){
            pos1[params$series[i]] <- posSizes[params$series[i]] # long
            
            if (initialLongPrice3[params$series[i]] != 0) {
              longCount3[params$series[i]] <<- longCount3[params$series[i]] + 1
              initialLongPrice3[params$series[i]] <<- (initialLongPrice3[params$series[i]]*longCount3[params$series[i]] + cl)/(longCount3[params$series[i]] + 1)
            }
            else {
              initialLongPrice3[params$series[i]] <<- cl
            }
            
            currentCashFlow1[store$iter,params$series[i]] <- -cl*posSizes[params$series[i]]
            #cat("currentCashFlow12:",currentCashFlow12[store$iter,params$series[i]],"\n")
          }         
        }
        #When in overbought price level
        if (R1 > -50 + params$threshold1_wr[i]){
          #countOB[i] <<- countOB[i] + 1
          #when there's a bearish crossover
          if (last(MAclS1) < last(MAclL1) && last(MAclS1,n=2)[-2] > last(MAclL1,n=2)[-2] 
              || last(macd1) < last(signal1) && last(macd1,n=2[-2]) > last(signal1,n=2)[-2]){
            pos1[params$series[i]] <- -posSizes[params$series[i]] # short
            
            if (initialShortPrice3[params$series[i]] != 0) {
              shortCount3[params$series[i]] <<- shortCount3[params$series[i]] + 1
              initialShortPrice3[params$series[i]] <<- (initialShortPrice3[params$series[i]]*shortCount3[params$series[i]] + cl)/(shortCount3[params$series[i]] + 1)
            }
            else {
              initialShortPrice3[params$series[i]] <<- cl
            }
            
            currentCashFlow1[store$iter,params$series[i]] <- cl*posSizes[params$series[i]]
            #cat("currentCashFlow12:",currentCashFlow12[store$iter,params$series[i]],"\n")
          }
        }
        
        marketOrders1[params$series[i]] <- marketOrders1[params$series[i]] + pos1[params$series[i]]
        currentPos1[params$series[i]] <<- currentPos1[params$series[i]] + marketOrders1[params$series[i]]
        cashFlow1[store$iter,params$series[i]] <<- cashFlow1[(store$iter-1),params$series[i]] + currentCashFlow1[store$iter,params$series[i]] + currentCashFlow11[store$iter,params$series[i]]
        #cat("cashFlow1:",cashFlow1[store$iter,params$series[i]],"\n")
        #PnLOld[params$series[i]] <- PnL[params$series[i]] #Yesterday's profit/loss
        currentWorth1[params$series[i]] <- cl*currentPos1[params$series[i]]
        PnLNew1[params$series[i]] <- currentWorth1[params$series[i]] +  cashFlow1[store$iter,params$series[i]]
        #cat("PnLNew1[params$series[i]]",PnLNew1[params$series[i]],"\n")
        #PnLCurrent[store$iter,params$series[i]] <<- PnLNew[params$series[i]] - PnLOld[params$series[i]]
        CPnL1[store$iter,params$series[i]] <<- PnLNew1[params$series[i]]
        MAPnL1[params$series[i]] <- last(SMA(CPnL1[startIndexWRL1:store$iter,params$series[i]]))
        
        cumPnL1 <<- CPnL1[1:store$iter,params$series[i]]
        #cat("cumPnL1:",CPnL1[1:store$iter,params$series[i]],"\n")
        #cat("MAX cumPnL1:",max(cumPnL1),"\n")
        maxDrawdown1[store$iter,params$series[i]] <<- max(max(cumPnL1)-cumPnL1,maxDrawdown1[store$iter-1,params$series[i]])
        #cat("maxDrawdown1",maxDrawdown1[store$iter,],"\n")
        PD1 <- ifelse(CPnL1[store$iter,params$series[i]] > 0, 
                      CPnL1[store$iter,params$series[i]]/maxDrawdown1[store$iter,params$series[i]], 
                      CPnL1[store$iter,params$series[i]])
        #cat("PD1:",PD1,"\n")
        ######################################################################################################
        
        highest2 <- max(store$hi[startIndexWRL2:store$iter,i])
        lowest2 <- min(store$lo[startIndexWRL2:store$iter,i])
        R2 <- (highest2 - cl)/(highest2 - lowest2)*(-100)
        #cat("W%R: ",R,"\n")
        
        macdData2 <- MACD(store$cl[startIndex2:store$iter,i],
                          nSlow=params$lookbackL2[i],nFast=params$lookbackS2[i],
                          nSig=round((params$lookbackL2[i]+params$lookbackS2[i])/2))
        macd2 <- macdData2[,1]
        signal2 <- macdData2[,2]
        
        MAclS2 <- SMA(store$cl[(startIndexS2-1):store$iter,i],n=params$lookbackS2[i])
        MAclL2 <- SMA(store$cl[startIndexL2:store$iter,i],n=params$lookbackL2[i])
        
        #Take a positin when signel line cross(as soon as the trend acceleration appears)
        #if (params$nWait == 0){
        #in a oversold/overbought price level
        if (R2 < -50 - params$threshold2_wr[i]){
          #countOS[i] <<- countOS[i] + 1
          #when there's a bullish crossover
          if (last(MAclS2) > last(MAclL2) && last(MAclS2,n=2)[-2] < last(MAclL2,n=2)[-2] 
              || last(macd2) > last(signal2) && last(macd2,n=2)[-2] < last(signal2,n=2)[-2]){
            pos2[params$series[i]] <- posSizes[params$series[i]] # long
            
            if (initialLongPrice4[params$series[i]] != 0) {
              longCount4[params$series[i]] <<- longCount4[params$series[i]] + 1
              initialLongPrice4[params$series[i]] <<- (initialLongPrice4[params$series[i]]*longCount4[params$series[i]] + cl)/(longCount4[params$series[i]] + 1)
            }
            else {
              initialLongPrice4[params$series[i]] <<- cl
            }
            
            currentCashFlow2[store$iter,params$series[i]] <- -cl*posSizes[i]
          }         
        }
        #When in overbought price level
        if (R2 > -50 + params$threshold2_wr[i]){
          #countOB[i] <<- countOB[i] + 1
          #when there's a bearish crossover
          if (last(MAclS2) < last(MAclL2) && last(MAclS2,n=2)[-2] > last(MAclL2,n=2)[-2] 
              || last(macd2) < last(signal2) && last(macd2,n=2[-2]) > last(signal2,n=2)[-2]){
            pos2[params$series[i]] <- -posSizes[params$series[i]]  # short
            
            if (initialShortPrice4[params$series[i]] != 0) {
              shortCount4[params$series[i]] <<- shortCount4[params$series[i]] + 1
              initialShortPrice4[params$series[i]] <<- (initialShortPrice4[params$series[i]]*shortCount4[params$series[i]] + cl)/(shortCount4[params$series[i]] + 1)
            }
            else {
              initialShortPrice4[params$series[i]] <<- cl
            }
            
            currentCashFlow2[store$iter,params$series[i]] <- cl*posSizes[i]
          }
        } 
        marketOrders2[params$series[i]] <- marketOrders2[params$series[i]] + pos2[params$series[i]]
        currentPos2[params$series[i]] <<- currentPos2[params$series[i]] + marketOrders2[params$series[i]]
        
        cashFlow2[store$iter,params$series[i]] <<- cashFlow2[(store$iter-1),params$series[i]] + currentCashFlow2[store$iter,params$series[i]] + currentCashFlow21[store$iter,params$series[i]]
        #cat("cashFlow2:",cashFlow2[store$iter,params$series[i]],"\n")
        #PnLOld[params$series[i]] <- PnL[params$series[i]] #Yesterday's profit/loss
        currentWorth2[params$series[i]] <- cl*currentPos2[params$series[i]]
        PnLNew2[params$series[i]] <- currentWorth2[params$series[i]] +  cashFlow2[params$series[i]]
        #cat("PnLNew2[params$series[i]]",PnLNew2[params$series[i]],"\n")
        #PnLCurrent[store$iter,params$series[i]] <<- PnLNew[params$series[i]] - PnLOld[params$series[i]]
        CPnL2[store$iter,params$series[i]] <<- PnLNew2[params$series[i]]
        #cat("CPnL1[startIndexWRL1:store$iter,params$series[i]]:",CPnL1[startIndexWRL1:store$iter,params$series[i]],"\n")
        #cat("CPnL2[startIndexWRL2:store$iter,params$series[i]]:",CPnL2[startIndexWRL2:store$iter,params$series[i]],"\n")
        MAPnL2[params$series[i]] <- last(SMA(CPnL2[startIndexWRL2:store$iter,params$series[i]]))
        
        cumPnL2 <<- CPnL2[1:store$iter,params$series[i]]
        #cat("cumPnL2:",CPnL2[1:store$iter,params$series[i]],"\n")
        #cat("MAX cumPnL2:",max(cumPnL2),"\n")
        maxDrawdown2[store$iter,params$series[i]] <<- max(max(cumPnL2)-cumPnL2,maxDrawdown2[store$iter-1,params$series[i]])
        #cat("maxDrawdown2",maxDrawdown2[store$iter,],"\n")
        PD2 <- ifelse(CPnL2[store$iter,params$series[i]] > 0, 
                      CPnL2[store$iter,params$series[i]]/maxDrawdown2[store$iter,params$series[i]], 
                      CPnL2[store$iter,params$series[i]])
        #cat("PD2:",PD2,"\n")
        ###########################################################################################
        cat("PD1:",PD1,"\n")
        cat("PD2:",PD2,"\n")
        ###########################################################################################
        if(PD1 >= PD2){
          #POS[params$series[i]] <- pos1[params$series[i]]
          marketOrders[params$series[i]] <- marketOrders1[params$series[i]]
          #currentCashFlow[store$iter,params$series[i]] <- currentCashFlow1[store$iter,params$series[i]]
        }
        else {
          #POS[params$series[i]] <- pos2[params$series[i]]
          marketOrders[params$series[i]] <- marketOrders2[params$series[i]]
          #currentCashFlow[store$iter,params$series[i]] <- currentCashFlow2[store$iter,params$series[i]]
        }
      }
      }
    }
    
  if (store$iter > max(maxLookback1,maxLookback2,params$lookback)) {
    for (i in 1:length(params$series)) {
      if(i==3|i==6)  {
        cl <- newRowList[[params$series[i]]]$Close
        
        if (currentPos3[params$series[i]] > 0) {
          if (cl >= initialLongPrice5[params$series[i]]*params$targetProfit) {
            marketOrders3[params$series[i]] <- -currentPos3[params$series[i]]
            currentCashFlow31[store$iter,params$series[i]] <- currentPos[params$series[i]]*cl
          }
        }
        
        if (currentPos3[params$series[i]] < 0) {
          if (cl <= initialShortPrice5[params$series[i]]*params$targetProfit) {
            marketOrders3[params$series[i]] <- -currentPos3[params$series[i]]
            currentCashFlow31[store$iter,params$series[i]] <- -currentPos[params$series[i]]*cl
          }
        }
      
        #############################################
        if (currentPos4[params$series[i]] > 0) {
          if (cl >= initialLongPrice6[params$series[i]]*params$targetProfit) {
            marketOrders4[params$series[i]] <- -currentPos4[params$series[i]]
            currentCashFlow41[store$iter,params$series[i]] <- currentPos[params$series[i]]*cl
          }
        }
        
        if (currentPos4[params$series[i]] < 0) {
          if (cl <= initialShortPrice6[params$series[i]]*params$targetProfit) {
            marketOrders4[params$series[i]] <- -currentPos4[params$series[i]]
            currentCashFlow41[store$iter,params$series[i]] <- -currentPos[params$series[i]]*cl
          }
        }
        
        ################################################
        if (currentPos5[params$series[i]] > 0) {
          if (cl >= initialLongPrice7[params$series[i]]*params$targetProfit) {
            marketOrders5[params$series[i]] <- -currentPos5[params$series[i]]
            currentCashFlow51[store$iter,params$series[i]] <- currentPos[params$series[i]]*cl
          }
        }
        
        if (currentPos5[params$series[i]] < 0) {
          if (cl <= initialShortPrice7[params$series[i]]*params$targetProfit) {
            marketOrders5[params$series[i]] <- -currentPos5[params$series[i]]
            currentCashFlow51[store$iter,params$series[i]] <- -currentPos[params$series[i]]*cl
          }
        }
        
        startIndex <-  store$iter - params$lookback
        openDiffs <- diff(store$op)
        absOpenDiffs <- as.matrix(abs(openDiffs))
        absOpenDiffs <- absOpenDiffs[1:store$iter,]
        #print(absOpenDiffs[1:store$iter,])
        avgAbsDiffs <- colMeans(absOpenDiffs)
        largestAvgAbsDiff <- max(avgAbsDiffs)
        posSizes[params$series[i]] <- round(largestAvgAbsDiff/avgAbsDiffs[params$series[i]])
        
        marketOrders[i] <- marketOrders3[i] <- marketOrders4[i] <- 0
        marketOrders5[i] <- -currentPos[i]
        lookback1 <- params$lookbackL1[i] + round((params$lookbackL1[i]+params$lookbackS1[i])/2)
        
        lookback2 <- params$lookbackL2[i] + round((params$lookbackL2[i]+params$lookbackS2[i])/2)
        startIndex <-  store$iter - params$lookback
        startIndex1 <- store$iter - lookback1
        print(startIndex1)
        startIndexWRL1 <- store$iter - params$lookbackWRL1[i]
        startIndexS1 <- store$iter - params$lookbackS1[i]
        startIndexL1 <- store$iter - params$lookbackL1[i]
        
        startIndex2 <- store$iter - lookback2
        startIndexWRL2 <- store$iter - params$lookbackWRL2[i]
        startIndexS2 <- store$iter - params$lookbackS2[i]
        startIndexL2 <- store$iter - params$lookbackL2[i]
        #posPnL[i] <- ifelse(PnLCurrent[store$iter,i] > 0,PnLCurrent[store$iter,i],1)
        #posPnL[i] <- ifelse(posPnL[i]*0.01 > 1,posPnL[i]*0.1,1) #scaling down
        
        
        #currentWorth[params$series[i]] <- cl*currentPos[params$series[i]] 
        
        
        highest1 <- max(store$hi[startIndexWRL1:store$iter,i])
        lowest1 <- min(store$lo[startIndexWRL1:store$iter,i])
        R1 <- (highest1 - cl)/(highest1 - lowest1)*(-100)
        #cat("W%R: ",R,"\n")
        
        macdData1 <- MACD(store$cl[startIndex1:store$iter,i],
                          nSlow=params$lookbackL1[i],nFast=params$lookbackS1[i],nSig=round((params$lookbackL1[i]+params$lookbackS1[i])/2))
        macd1 <- macdData1[,1]
        signal1 <- macdData1[,2]
        
        MAclS1 <- SMA(store$cl[(startIndexS1-1):store$iter,i],n=params$lookbackS1[i])
        MAclL1 <- SMA(store$cl[startIndexL1:store$iter,i],n=params$lookbackL1[i])
        
        #Take a positin when signel line cross(as soon as the trend acceleration appears)
        #if (params$nWait == 0){
        #in a oversold/overbought price level
        if (R1 < -50 - params$threshold1_wr[i]){
          #countOS[i] <<- countOS[i] + 1
          #when there's a bullish crossover
          if (last(MAclS1) > last(MAclL1) && last(MAclS1,n=2)[-2] < last(MAclL1,n=2)[-2] 
              || last(macd1) > last(signal1) && last(macd1,n=2)[-2] < last(signal1,n=2)[-2]){
            pos3[params$series[i]] <- posSizes[params$series[i]] # long
            
            if (initialLongPrice5[params$series[i]] != 0) {
              longCount5[params$series[i]] <<- longCount5[params$series[i]] + 1
              initialLongPrice5[params$series[i]] <<- (initialLongPrice5[params$series[i]]*longCount5[params$series[i]] + cl)/(longCount5[params$series[i]] + 1)
            }
            else {
              initialLongPrice5[params$series[i]] <<- cl
            }
            
            currentCashFlow3[store$iter,params$series[i]] <- -cl*posSizes[params$series[i]]
            #cat("currentCashFlow12:",currentCashFlow12[store$iter,params$series[i]],"\n")
          }         
        }
        #When in overbought price level
        if (R1 > -50 + params$threshold1_wr[i]){
          #countOB[i] <<- countOB[i] + 1
          #when there's a bearish crossover
          if (last(MAclS1) < last(MAclL1) && last(MAclS1,n=2)[-2] > last(MAclL1,n=2)[-2] 
              || last(macd1) < last(signal1) && last(macd1,n=2[-2]) > last(signal1,n=2)[-2]){
            pos3[params$series[i]] <- -posSizes[params$series[i]]  # short
            
            if (initialShortPrice5[params$series[i]] != 0) {
              shortCount5[params$series[i]] <<- shortCount5[params$series[i]] + 1
              initialShortPrice5[params$series[i]] <<- (initialShortPrice5[params$series[i]]*shortCount5[params$series[i]] + cl)/(shortCount5[params$series[i]] + 1)
            }
            else {
              initialShortPrice5[params$series[i]] <<- cl
            }
            
            currentCashFlow3[store$iter,params$series[i]] <- cl*posSizes[params$series[i]]
            #cat("currentCashFlow12:",currentCashFlow12[store$iter,params$series[i]],"\n")
          }
        }
        #print(cashFlow1[store$iter-1,params$series[i]])
        #print(cashFlow1[store$iter,params$series[i]])
        #print(currentCashFlow12[store$iter,params$series[i]])
        #cat("cashFlow1 before:",cashFlow1[(store$iter-1),params$series[i]],"\n")
        marketOrders3[params$series[i]] <- marketOrders3[params$series[i]] + pos3[params$series[i]]
        currentPos3[params$series[i]] <<- currentPos3[params$series[i]] + marketOrders3[params$series[i]]
        cashFlow3[store$iter,params$series[i]] <<- cashFlow3[(store$iter-1),params$series[i]] + currentCashFlow3[store$iter,params$series[i]] + currentCashFlow31[store$iter,params$series[i]]
        #cat("cashFlow1:",cashFlow1[store$iter,params$series[i]],"\n")
        #PnLOld[params$series[i]] <- PnL[params$series[i]] #Yesterday's profit/loss
        currentWorth3[params$series[i]] <- cl*currentPos3[params$series[i]]
        PnLNew3[params$series[i]] <- currentWorth3[params$series[i]] +  cashFlow3[store$iter,params$series[i]]
        #cat("PnLNew1[params$series[i]]",PnLNew1[params$series[i]],"\n")
        #PnLCurrent[store$iter,params$series[i]] <<- PnLNew[params$series[i]] - PnLOld[params$series[i]]
        CPnL3[store$iter,params$series[i]] <<- PnLNew3[params$series[i]]
        MAPnL3[params$series[i]] <- last(SMA(CPnL3[startIndexWRL1:store$iter,params$series[i]]))
        
        cumPnL3 <<- CPnL3[1:store$iter,params$series[i]]
        #cat("cumPnL1:",CPnL1[1:store$iter,params$series[i]],"\n")
        #cat("MAX cumPnL1:",max(cumPnL1),"\n")
        maxDrawdown3[store$iter,params$series[i]] <<- max(max(cumPnL3)-cumPnL3,maxDrawdown3[store$iter-1,params$series[i]])
        #cat("maxDrawdown1",maxDrawdown1[store$iter,],"\n")
        PD3 <- ifelse(CPnL3[store$iter,params$series[i]] > 0, 
                      CPnL3[store$iter,params$series[i]]/maxDrawdown3[store$iter,params$series[i]], 
                      CPnL3[store$iter,params$series[i]])
        #cat("PD1:",PD1,"\n")
        ######################################################################################################
        
        highest2 <- max(store$hi[startIndexWRL2:store$iter,i])
        lowest2 <- min(store$lo[startIndexWRL2:store$iter,i])
        R2 <- (highest2 - cl)/(highest2 - lowest2)*(-100)
        #cat("W%R: ",R,"\n")
        
        macdData2 <- MACD(store$cl[startIndex2:store$iter,i],
                          nSlow=params$lookbackL2[i],nFast=params$lookbackS2[i],
                          nSig=round((params$lookbackL2[i]+params$lookbackS2[i])/2))
        macd2 <- macdData2[,1]
        signal2 <- macdData2[,2]
        
        MAclS2 <- SMA(store$cl[(startIndexS2-1):store$iter,i],n=params$lookbackS2[i])
        MAclL2 <- SMA(store$cl[startIndexL2:store$iter,i],n=params$lookbackL2[i])
        
        #Take a positin when signel line cross(as soon as the trend acceleration appears)
        #if (params$nWait == 0){
        #in a oversold/overbought price level
        if (R2 < -50 - params$threshold2_wr[i]){
          #countOS[i] <<- countOS[i] + 1
          #when there's a bullish crossover
          if (last(MAclS2) > last(MAclL2) && last(MAclS2,n=2)[-2] < last(MAclL2,n=2)[-2] 
              || last(macd2) > last(signal2) && last(macd2,n=2)[-2] < last(signal2,n=2)[-2]){
            pos4[params$series[i]] <- posSizes[params$series[i]] # long
            
            if (initialLongPrice6[params$series[i]] != 0) {
              longCount6[params$series[i]] <<- longCount6[params$series[i]] + 1
              initialLongPrice6[params$series[i]] <<- (initialLongPrice6[params$series[i]]*longCount6[params$series[i]] + cl)/(longCount6[params$series[i]] + 1)
            }
            else {
              initialLongPrice6[params$series[i]] <<- cl
            }
            
            currentCashFlow4[store$iter,params$series[i]] <- -cl*posSizes[i]
          }         
        }
        #When in overbought price level
        if (R2 > -50 + params$threshold2_wr[i]){
          #countOB[i] <<- countOB[i] + 1
          #when there's a bearish crossover
          if (last(MAclS2) < last(MAclL2) && last(MAclS2,n=2)[-2] > last(MAclL2,n=2)[-2] 
              || last(macd2) < last(signal2) && last(macd2,n=2[-2]) > last(signal2,n=2)[-2]){
            pos4[params$series[i]] <- -posSizes[params$series[i]]  # short
            
            if (initialShortPrice6[params$series[i]] != 0) {
              shortCount6[params$series[i]] <<- shortCount6[params$series[i]] + 1
              initialShortPrice6[params$series[i]] <<- (initialShortPrice6[params$series[i]]*shortCount6[params$series[i]] + cl)/(shortCount6[params$series[i]] + 1)
            }
            else {
              initialShortPrice6[params$series[i]] <<- cl
            }
            
            currentCashFlow4[store$iter,params$series[i]] <- cl*posSizes[i]
          }
        } 
        marketOrders4[params$series[i]] <- marketOrders4[params$series[i]] + pos4[params$series[i]]
        currentPos4[params$series[i]] <<- currentPos4[params$series[i]] + marketOrders4[params$series[i]]
        
        cashFlow4[store$iter,params$series[i]] <<- cashFlow4[(store$iter-1),params$series[i]] + currentCashFlow4[store$iter,params$series[i]] + currentCashFlow41[store$iter,params$series[i]]
        #cat("cashFlow2:",cashFlow2[store$iter,params$series[i]],"\n")
        #PnLOld[params$series[i]] <- PnL[params$series[i]] #Yesterday's profit/loss
        currentWorth4[params$series[i]] <- cl*currentPos4[params$series[i]]
        PnLNew4[params$series[i]] <- currentWorth4[params$series[i]] +  cashFlow4[params$series[i]]
        #cat("PnLNew2[params$series[i]]",PnLNew2[params$series[i]],"\n")
        #PnLCurrent[store$iter,params$series[i]] <<- PnLNew[params$series[i]] - PnLOld[params$series[i]]
        CPnL4[store$iter,params$series[i]] <<- PnLNew4[params$series[i]]
        #cat("CPnL1[startIndexWRL1:store$iter,params$series[i]]:",CPnL1[startIndexWRL1:store$iter,params$series[i]],"\n")
        #cat("CPnL2[startIndexWRL2:store$iter,params$series[i]]:",CPnL2[startIndexWRL2:store$iter,params$series[i]],"\n")
        MAPnL4[params$series[i]] <- last(SMA(CPnL4[startIndexWRL2:store$iter,params$series[i]]))
        
        cumPnL4 <<- CPnL4[1:store$iter,params$series[i]]
        #cat("cumPnL2:",CPnL2[1:store$iter,params$series[i]],"\n")
        #cat("MAX cumPnL2:",max(cumPnL2),"\n")
        maxDrawdown4[store$iter,params$series[i]] <<- max(max(cumPnL4)-cumPnL4,maxDrawdown4[store$iter-1,params$series[i]])
        #cat("maxDrawdown2",maxDrawdown2[store$iter,],"\n")
        PD4 <- ifelse(CPnL4[store$iter,params$series[i]] > 0, 
                      CPnL4[store$iter,params$series[i]]/maxDrawdown4[store$iter,params$series[i]], 
                      CPnL4[store$iter,params$series[i]])
        
        
        
        ########################################################
        
        cl <- newRowList[[params$series[i]]]$Close
        rocCalc <- cl - store$cl[store$iter-params$rocCalLength,i]
        
        bbands <- last(BBands(store$cl[startIndex:store$iter,i],
                              n=params$lookback,sd=params$sdParam))
        
        
        #entry 
        averageLiq <- last(SMA((store$cl[,i]),n=liqDay))
        if(currentPos[i]==0){
          if (cl > bbands["up"]&rocCalc>0) {
            pos5[params$series[i]] <- posSizes[params$series[i]]
            
            if (initialLongPrice7[params$series[i]] != 0) {
              longCount7[params$series[i]] <<- longCount7[params$series[i]] + 1
              initialLongPrice7[params$series[i]] <<- (initialLongPrice7[params$series[i]]*longCount7[params$series[i]] + cl)/(longCount7[params$series[i]] + 1)
            }
            else {
              initialLongPrice7[params$series[i]] <<- cl
            }
            
            liqDay<- 50              
            currentCashFlow5[store$iter,params$series[i]] <- -cl*posSizes[params$series[i]]
            # print(1)
          }
          else if (cl < bbands["dn"]&rocCalc<0) {
            pos5[params$series[i]] <- -posSizes[params$series[i]]
            
            if (initialShortPrice7[params$series[i]] != 0) {
              shortCount7[params$series[i]] <<- shortCount7[params$series[i]] + 1
              initialShortPrice7[params$series[i]] <<- (initialShortPrice7[params$series[i]]*shortCount7[params$series[i]] + cl)/(shortCount7[params$series[i]] + 1)
            }
            else {
              initialShortPrice7[params$series[i]] <<- cl
            }
            
            liqDay<- 50           
            currentCashFlow5[store$iter,params$series[i]] <- cl*posSizes[params$series[i]]
            # print(2)
          }
        }
        
        #short
        averageLiq <- last(SMA((store$cl[,i]),n=liqDay))
        if(currentPos[i]!=0&&cl<averageLiq){
          pos5[params$series[i]] <- -posSizes[params$series[i]]
          
          if (initialShortPrice7[params$series[i]] != 0) {
            shortCount7[params$series[i]] <<- shortCount7[params$series[i]] + 1
            initialShortPrice7[params$series[i]] <<- (initialShortPrice7[params$series[i]]*shortCount7[params$series[i]] + cl)/(shortCount7[params$series[i]] + 1)
          }
          else {
            initialShortPrice7[params$series[i]] <<- cl
          }
          
          liqDay<-liqDay-1
          liqDay<-max(liqDay,10) 
          
          
          currentCashFlow5[store$iter,params$series[i]] <- cl*posSizes[params$series[i]]
          # print(3)
        } 
        #long
        if(currentPos[i]!=0&&cl>averageLiq){
          pos5[params$series[i]] <- posSizes[params$series[i]]
          
          if (initialLongPrice7[params$series[i]] != 0) {
            longCount7[params$series[i]] <<- longCount7[params$series[i]] + 1
            initialLongPrice7[params$series[i]] <<- (initialLongPrice7[params$series[i]]*longCount7[params$series[i]] + cl)/(longCount7[params$series[i]] + 1)
          }
          else {
            initialLongPrice7[params$series[i]] <<- cl
          }
          
          liqDay<-liqDay-1
          liqDay<-max(liqDay,10)  
          currentCashFlow5[store$iter,params$series[i]] <- -cl*posSizes[params$series[i]]
          # print(4)
        }
        marketOrders5[params$series[i]] <- marketOrders5[params$series[i]] + pos5[params$series[i]]
        currentPos5[params$series[i]] <<- currentPos5[params$series[i]] + marketOrders5[params$series[i]]
        
        cashFlow5[store$iter,params$series[i]] <<- cashFlow5[(store$iter-1),params$series[i]] + currentCashFlow5[store$iter,params$series[i]] + currentCashFlow51[store$iter,params$series[i]]
        #cat("cashFlow2:",cashFlow2[store$iter,params$series[i]],"\n")
        #PnLOld[params$series[i]] <- PnL[params$series[i]] #Yesterday's profit/loss
        currentWorth5[params$series[i]] <- cl*currentPos5[params$series[i]]
        PnLNew5[params$series[i]] <- currentWorth5[params$series[i]] +  cashFlow5[params$series[i]]
        #cat("PnLNew2[params$series[i]]",PnLNew2[params$series[i]],"\n")
        #PnLCurrent[store$iter,params$series[i]] <<- PnLNew[params$series[i]] - PnLOld[params$series[i]]
        CPnL5[store$iter,params$series[i]] <<- PnLNew5[params$series[i]]
        #cat("CPnL1[startIndexWRL1:store$iter,params$series[i]]:",CPnL1[startIndexWRL1:store$iter,params$series[i]],"\n")
        #cat("CPnL2[startIndexWRL2:store$iter,params$series[i]]:",CPnL2[startIndexWRL2:store$iter,params$series[i]],"\n")
        MAPnL5[params$series[i]] <- last(SMA(CPnL5[startIndex:store$iter,params$series[i]]))
        
        cumPnL5 <<- CPnL5[1:store$iter,params$series[i]]
        #cat("cumPnL2:",CPnL2[1:store$iter,params$series[i]],"\n")
        #cat("MAX cumPnL2:",max(cumPnL2),"\n")
        maxDrawdown5[store$iter,params$series[i]] <<- max(max(cumPnL5)-cumPnL5,maxDrawdown5[store$iter-1,params$series[i]])
        #cat("maxDrawdown2",maxDrawdown2[store$iter,],"\n")
        PD5 <- ifelse(CPnL5[store$iter,params$series[i]] > 0, 
                      CPnL5[store$iter,params$series[i]]/maxDrawdown5[store$iter,params$series[i]], 
                      CPnL5[store$iter,params$series[i]])
        
        ###########################################################################################
        cat("PD1:",PD1,"\n")
        cat("PD2:",PD2,"\n")
        ###########################################################################################
        if(PD3 >= PD4) {
          if(PD3 >= PD5) {
            #POS[params$series[i]] <- pos1[params$series[i]]
            marketOrders[params$series[i]] <- marketOrders3[params$series[i]]
            #currentCashFlow[store$iter,params$series[i]] <- currentCashFlow3[store$iter,params$series[i]] 
          }
          
          else{
            marketOrders[params$series[i]] <- marketOrders5[params$series[i]]
            #currentCashFlow[store$iter,params$series[i]] <- currentCashFlow5[store$iter,params$series[i]] 
          } 
        }
        
        else {
          if(PD4 >= PD5)
            #POS[params$series[i]] <- pos2[params$series[i]]
          {marketOrders[params$series[i]] <- marketOrders4[params$series[i]]
           #currentCashFlow[store$iter,params$series[i]] <- currentCashFlow4[store$iter,params$series[i]] 
          }
          else{
            marketOrders[params$series[i]] <- marketOrders5[params$series[i]]
            #currentCashFlow[store$iter,params$series[i]] <- currentCashFlow5[store$iter,params$series[i]]
          }
        }
      }
    }
  }
  print(posSizes)
  #cashFlow <<- cashFlow + currentCashFlow
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
