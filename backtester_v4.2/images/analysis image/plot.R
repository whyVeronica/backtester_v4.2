source('framework/data.R');
dataList <- getData(directory="PART2")

d <- dataList[[10]][1:1100,1:4]

chartSeries(d,type="candlesticks",theme="white",name="Series10")