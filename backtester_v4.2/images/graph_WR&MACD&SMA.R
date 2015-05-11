source('framework/data.R');
dataList <- getData(directory="PART1")

d <- dataList[[7]]
d <- d[80:180,1:4]
cl <- d$"Close"

hi <- d$"High"
lo <- d$"Low"
lookbackR <- 40
highest <- runMax(hi,lookbackR)
lowest <- runMin(lo,lookbackR)
R <- (highest - cl)/(highest - lowest)*(-100)
head(R)
#b <- rep(-30,length(cl))
#names(b) <- names(R)
#overSold <- rep(-70,length(cl))
MACD_slowLine <- EMA(cl,n=10)
MACD_fastLine <- EMA(cl,n=8)
MACD <- MACD_slowLine - MACD_fastLine
signalLine <- EMA(MACD,n=9)
tail(signalLine)

SMA_slowLine <- SMA(cl,n=10)
SMA_fastLine <- SMA(cl,n=8)

#RSI <- RSI(cl,n=14)

chartSeries(d,type="candlesticks",theme="white",name="Series7")
addTA(SMA_fastLine,col="green",lwd=2,on=1)
addTA(SMA_slowLine,col="orange",lwd=2,on=1)
addTA(MACD,col="red",lwd=2,on=NA)
addTA(signalLine,col="blue",lwd=2,on=2)
addTA(R,lwd=2,on=NA)
#addTA(overBought,lwd=2,col="red",on=3)
#addTA(overSold,lwd=2,col="blue",on=3)
