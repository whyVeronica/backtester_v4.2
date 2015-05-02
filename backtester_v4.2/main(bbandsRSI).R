source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 

# To change things:  comment out the current choice 
# and uncomment a different choice

# DATA ##################################
dataList <- getData(directory="PART1")

#####################################
# Strategies using only market orders
#####################################
#strategyFile <-'strategies/fixed.R'
#strategyFile <-'strategies/copycat.R'
#strategyFile <-'strategies/random.R'
#strategyFile <-'strategies/rsiRSI.R'
#strategyFile <-'strategies/rsi2.R'
#strategyFile <-'strategies/bbands.R'
strategyFile <-'strategies/bbandsRSI.R'
#strategyFile <-'strategies/bbands_holding.R'
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

#params <- list(lookback=14,lookback1=6, lookback2=14, threshold=25) # rsiRSI
params <- list(lookback=20,threshold=25,series=1:10,posSizes=rep(1,10))#bbandsRSI
# params for bbands strategy
#params <- list(lookback=20,sdParam=1.5,
               #series=1:4,posSizes=rep(1,10))
#params <- list(lookback=50,sdParam=1.5,
                   #series=c(1,3,5,7,8,9),posSizes=rep(1,10))
# with holding period
# params <- list(lookback=50,sdParam=1.5,series=c(1,3),
#                         posSizes=rep(1,5),holdPeriod=6)

#params		<- list(spreadPercentage=0.0001,inventoryLimits=rep(3,10),series=9:10) # params for limit strategy

print("Parameters:")
print(params)

# BACKTEST PARAMETERS ##########################
# split data in two (e.g. for in/out test)
numDays <- nrow(dataList[[1]])
inSampDays <- 200 

# in-sample period
dataList <- lapply(dataList, function(x) x[1:inSampDays])

# out-of-sample period
#dataList <- lapply(dataList, function(x) 
#                               x[(inSampDayss+1):numDays)

sMult <- 0.20 # slippage multiplier

# DO BACKTEST ############################
results <- backtest(dataList,getOrders,params,sMult)
pfolioPnL <- plotResults(dataList,results,plotType='ggplot2')
