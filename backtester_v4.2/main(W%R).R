  source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 

# To change things:  comment out the current choice 
# and uncomment a different choice

# DATA ##################################
dataList <- getData(directory="PART2")

#####################################
# Strategies using only market orders
#####################################
#strategyFile <-'strategies/MACD.R'
#strategyFile <-'strategies/W%R(pos-mean abs diff)2.R'
strategyFile <-'strategies/W%R(pos-P&R)4.R'
#strategyFile <-'strategies/W%R(pos-open).R'
###############################
# Strategies using Limit orders
###############################
#strategyFile <-'strategies/simple_limit.R'

cat("Sourcing",strategyFile,"\n")
source(strategyFile) # load in getOrders

# STRATEGY PARAMETERS ###########################

#params <- list(sizes=rep(1,10)) # fixed
#params <- list(sizes=c(0,0,1,0,0,0,0,0,0,0)) # fixed
#params <- list(sizes=c(1,4,-2,1,1,2,2,-2,-2,1)) # fixed

# params <- NULL # copycat

# params <- list(maxLots=100) # random

# params <- list(lookback=10,threshold=25) # rsi
# params <- list(lookback=20,threshold=25,series=1:10,posSizes=rep(1,10)) #rsi2

# params for bbands strategy
#params <- list(lookback=20,sdParam=1.5,
                   #series=1:4,posSizes=rep(1,10))
#params <- list(lookback=50,sdParam=1.5,
                   #series=c(1,3,5,7,8,9),posSizes=rep(1,10))
#params <- list(lookback=20,sdParam=1.5,
               #series=1:10,posSizes=rep(1,10))
# with holding period
# params <- list(lookback=50,sdParam=1.5,series=c(1,3),
#                         posSizes=rep(1,5),holdPeriod=6)

#params		<- list(spreadPercentage=0.005,inventoryLimits=rep(50,10),series=1:10) # params for limit strategy
#params <- list(nFast=9, nSlow=26, nSig=12, nHold=3, series=1:10, posSizes=rep(1,10)) #macd 

params <- list(lookbackR=14,lookbackS=5,lookbackL=15,lookbackLimit=10,threshold=30,
               spreadPercentage=0.005,inventoryLimits=rep(100,10),series=1:10,posSizes=rep(1,10)) #Williams%R.R
  
#params <- list(lookbackR=14,lookbackS=5,lookbackL=30,threshold=30,
                 #inventoryLimits=rep(100,10),series=c(5,7,9,10),posSizes=rep(1,10))
#params <- list(lookbackR=14,threshold=30,inventoryLimits=rep(100,10),series=1:10,posSizes=rep(1,10)) #Williams%R.R

#params <- list(lookbackS=14,lookbackL=200,threshold=30,nFast=12, nSlow=26, nSig=9, nHold=3, series=1:10, 
#               inventoryLimits=rep(300,10)) #W%R & macd 

print("Parameters:")
print(params)

# BACKTEST PARAMETERS ##########################
# split data in two (e.g. for in/out test)
numDays <- nrow(dataList[[1]])
inSampDays <- 1100

# in-sample period
dataList <- lapply(dataList, function(x) x[1:inSampDays])

# out-of-sample period
#dataList <- lapply(dataList, function(x) x[(inSampDays+1):numDays])

sMult <- 0.2 # slippage multiplier

# DO BACKTEST ############################
results <- backtest(dataList,getOrders,params,sMult)
pfolioPnL <- plotResults(dataList,results,plotType='ggplot2')

# create a pdf image file
#dev.copy(pdf, file="images/Williams-R_14-200-30-all-all1-clearInv3-POSopen.pdf")
#dev.off()
