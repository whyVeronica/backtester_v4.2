source('framework/data.R');
library(caTools)
dataList <- getData(directory="PART1")

d <- dataList[[2]]
d <- d[100:300,1:4]
cl <- d$"Close"
hi <- d$"High"
lo <- d$"Low"

lookback <- 20
highest <- runmax(hi,lookback)
lowest <- runmin(lo,lookback)
R <- (highest - cl)/(highest - lowest)*(-100)
#print(R)

chartSeries(d,type="candlesticks",theme="white",name="Series2")
addTA(R,on=NA)



