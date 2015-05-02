source('framework/data.R');
library(caTools)
dataList <- getData(directory="PART1")

d <- dataList[[4]]
d <- d[100:300,4]

rsi <- RSI(d, n = 34)

chartSeries(d,type="candlesticks",theme="white",name="Series4")
addTA(rsi,on=NA)



