source('framework/data.R');
dataList <- getData(directory="PART1")

d <- dataList[[1]]
d <- d[300:500,1:4]
cl <- d$"Close"

hi <- d$"High"
lo <- d$"Low"
lookbackR <- 12
highest <- runMax(hi,lookbackR)
lowest <- runMin(lo,lookbackR)
R <- (highest - cl)/(highest - lowest)*(-100)
head(R)
#b <- rep(-30,length(cl))
#names(b) <- names(R)
#overSold <- rep(-70,length(cl))
MACD_slowLine <- EMA(cl,n=15)
MACD_fastLine <- EMA(cl,n=11)
MACD <- MACD_slowLine - MACD_fastLine
signalLine <- EMA(MACD,n=13)
tail(signalLine)

SMA_slowLine <- SMA(cl,n=15)
SMA_fastLine <- SMA(cl,n=11)

RSI <- RSI(cl,n=14)

chartSeries(d,type="candlesticks",theme="white",name="Series1")
addTA(SMA_fastLine,col="green",lwd=2,on=1)
addTA(SMA_slowLine,col="orange",lwd=2,on=1)
addTA(MACD,col="red",lwd=2,on=NA)
addTA(signalLine,col="blue",lwd=2,on=2)
addTA(R,lwd=2,on=NA)
#addTA(overBought,lwd=2,col="red",on=3)
#addTA(overSold,lwd=2,col="blue",on=3)
