source('framework/data.R');
dataList <- getData(directory="PART1")

d3 <- dataList[[5]]
d3 <- d3[200:300,1:4]
cl <- d3$"Close"

slowLine <- EMA(cl,n=5)
fastLine <- EMA(cl,n=15)
MACD <- slowLine - fastLine
signalLine <- EMA(MACD,n=10)


chartSeries(d3,type="candlesticks",theme="white",name="Series5")
addTA(slowLine,col='orange',on=1)
addTA(fastLine,col='green',on=1)
addTA(MACD,type="hist",col='grey',on=NA)
addTA(MACD,col='red',on=2)
addTA(signalLine,col='blue',on=2)

