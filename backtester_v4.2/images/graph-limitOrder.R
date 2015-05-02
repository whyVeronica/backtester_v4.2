source('framework/data.R');
library(caTools)
dataList <- getData(directory="PART1")

d <- dataList[[3]]
d <- d[1:500,1:4]
cl <- d$"Close"
hi <- d$"High"
lo <- d$"Low"

lookback <- 14
highest <- runmax(hi,lookback)
lowest <- runmin(lo,lookback)
maxLogRet <- runmax(abs(diff(log(cl))),lookback)
spread <- maxLogRet*10*(highest - lowest)

#spread <- 0.01*10*(hi-lo)

upper <- cl + spread/2
lower <- cl - spread/2


chartSeries(d,type="auto",theme="white",name="Series3")
addTA(upper,on=1)
addTA(lower,on=1)
