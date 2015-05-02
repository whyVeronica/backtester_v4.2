#Take a position after the trend has been held for one more day
if (params$nWait == 1){
  #When oversold 
  if (R < -50 - params$threshold){
    #when there's a bullish crossover that have held for one day
    if (last(MAclS) > last(MAclL) && last(MAclS,n=2)[-2] < last(MAclL,n=2)[-2] 
        &&  last(MAclS,n=3)[-c(2,3)] < last(MAclL,n=3)[-c(2,3)]
        || last(macd) > last(signal) && last(macd,n=2)[-2] > last(signal,n=2)[-2]
        && last(macd,n=3)[-c(2,3)] < last(signal,n=3)[-c(2,3)]){
      pos[params$series[i]] <- positionSizes[i] # long
      currentCashFlow[params$series[i]] <- -cl*positionSizes[i]
    }
  }
  #When overbought 
  if (R > -50 + params$threshold){
    #when there's a bearish crossover that have held for one day
    if (last(MAclS) < last(MAclL) && last(MAclS,n=2)[-2] > last(MAclL,n=2)[-2] 
        &&  last(MAclS,n=3)[-c(2,3)] > last(MAclL,n=3)[-c(2,3)]
        || last(macd) < last(signal) && last(macd,n=2)[-2] < last(signal,n=2)[-2]
        && last(macd,n=3)[-c(2,3)] > last(signal,n=3)[-c(2,3)]) {
      pos[params$series[i]] <- -positionSizes[i]  # short
      currentCashFlow[params$series[i]] <- cl*positionSizes[i]
    } 
  }
}