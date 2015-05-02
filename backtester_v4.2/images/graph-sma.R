source('framework/data.R');
dataList <- getData(directory="PART1")

d3 <- dataList[[9]]
d3 <- d3[1:550,1:4]
cl <- d3$"Close"

slowLine <- SMA(cl,n=38)
fastLine <- SMA(cl,n=9)


chartSeries(d3,type="candlesticks",theme="white",name="Series9")
addTA(slowLine,col='orange',lwd=2,on=1)
addTA(fastLine,col='green',lwd=2,on=1)
