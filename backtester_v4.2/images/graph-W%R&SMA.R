source('framework/data.R');
library(caTools)
dataList <- getData(directory="PART1")

d <- dataList[[1]]
d <- d[1:200,1:4]
cl <- d$"Close"
hi <- d$"High"
lo <- d$"Low"

lookbackR <- 14
highest <- runmax(hi,lookbackR)
lowest <- runmin(lo,lookbackR)
R <- (highest - cl)/(highest - lowest)*(-100)

lookbackS <- 5
lookbackL <- 30
MAclS <- SMA(cl,n=lookbackS)
MAclL <- SMA(cl,n=lookbackL)

chartSeries(d,type="candlesticks",theme="white",name="Series3")
addTA(R,on=NA)
addTA(MAclS,col="red",lwd=2,on=1)
addTA(MAclL,col="blue",lwd=2,on=1)

