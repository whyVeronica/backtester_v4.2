source("framework/data.R")

dataList <- getData(directory='PART1')

Macd <- function(series=1,nFast=4,nSlow=10,nSig=5){
  
  dataCl <- dataList[[series]][1:550,"Close"]
  pos <- rep(0,length=length(dataCl))
  
  lookback <- nSlow + nSig -1
  
  macdData <- MACD(dataCl,nFast=nFast,nSlow=nSlow,nSig=nSig)
  print(macdData)
  
  macd <- macdData$macd
  signal <- macdData$signal
  nActiveDays <- 0
  
  for (iter in 1:length(dataCl)) {
    
    if (iter>lookback){
      
      if (macd[iter] > signal[iter] && macd[iter-1] < signal[iter-1]){
        pos[iter] <- 1
        nActiveDays <- nActiveDays + 1
      }
      
      if (macd[iter] < signal[iter] && macd[iter-1] > signal[iter-1]){
        pos[iter] <- -1
        nActiveDays <- nActiveDays + 1
      }
    }
  }
  cat("Position: ", pos,"\n")
  cat("Active ratio is:", nActiveDays/length(dataCl)*100,"%","\n")
}

Macd(6,10,22,8)
