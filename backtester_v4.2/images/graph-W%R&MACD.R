source('framework/data.R')
library(caTools)
dataList <- getData(directory="PART1")

d <- dataList[[3]]
d <- d[1:200,1:4]
cl <- d$"Close"
hi <- d$"High"
lo <- d$"Low"

lookbackR <- 14
highest <- runmax(hi,lookbackR)
lowest <- runmin(lo,lookbackR)
R <- (highest - cl)/(highest - lowest)*(-100)

slowLine <- EMA(cl,n=5)
fastLine <- EMA(cl,n=10)
MACD <- slowLine - fastLine
signalLine <- EMA(MACD,n=5)

chartSeries(d,type="candlesticks",theme="white",name="Series3")
addTA(R,on=NA)
addTA(MACD,type="hist",col='grey',on=NA)
addTA(MACD,col="red",lwd=2,on=3)
addTA(signalLine,col="blue",lwd=2,on=3)
 
