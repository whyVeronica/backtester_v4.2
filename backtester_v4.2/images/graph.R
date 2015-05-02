source('framework/data.R');
dataList <- getData(directory="PART1")
d<-dataList[[1]]
d<-d[,"Close"]
bbands <- BBands(d,n=50,sd=1.25)
bup<-bbands[50:1100,"up"]
bdn<-bbands[50:1100,"dn"]
Cl_30Ago<-d[-(1:30),]
for(i in 1:length(Cl_30Ago))
{Cl_30Ago[i]=d[i]}

chartSeries(d,theme='white')
addTA(bup,col='blue',on=1)
addTA(bdn,col='red',on=1)
addTA(Cl_30Ago,col='yellow',on=1)