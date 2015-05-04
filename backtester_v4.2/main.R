source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 

# To change things:  comment out the current choice 
# and uncomment a different choice

# DATA ##################################
dataList <- getData(directory="PART3")


#####################################
# Strategies using only market orders
#####################################
#strategyFile <-'strategies/Modify/original.R'
#strategyFile <-'strategies/Modify/no2.R'
#strategyFile <-'strategies/Modify/P&R.R'
#strategyFile <-'strategies/Modify/Op.R'
#strategyFile <-'strategies/Modify/pos_MA_PD.R'
#strategyFile <-'strategies/Modify/no_pos.R'
#strategyFile <-'strategies/Modify/compareMAPD.R'
#strategyFile <-'strategies/Modify/3&6 - wan.R'
#strategyFile <-'strategies/Modify/3&6 - han.R'
#strategyFile <-'strategies/Modify/no2_stoploss+profittarget.R'
#strategyFile <-'strategies/Modify/pos_opendiff_risk=maxdrawdown.R'
#strategyFile <-'strategies/Modify/pos_op_risk=mdd.R'
strategyFile <-'strategies/Modify/wait.R'
###############################
# Strategies using Limit orders
###############################
#strategyFile <-'strategies/simple_limit.R'


cat("Sourcing",strategyFile,"\n")
source(strategyFile) # load in getOrders

# STRATEGY PARAMETERS ###########################
#params <- list(lookback=55,sdParam=2.5,
               #series=c(1:10),rocCalLength=30,posSizes=rep(1,10),
               #lookback_macd=14,lookback1_macd=5,
               #lookback2_macd=10,
               #threshold=15,nFast=5,
               #nSlow = 29, nSig = 8,
               #lookbackWRL1=c(20,10,10,0,34,30,42,0,37,35),lookbackWRL2=c(20,10,10,0,34,30,42,0,37,35),
               #lookbackS1=c(11,10,8,0,5,4,8,0,9,8), lookbackS2=c(11,10,8,0,5,4,8,0,9,8),
               #lookbackL1=c(15,42,40,0,15,17,10,0,38,10),lookbackL2=c(15,42,40,0,15,17,10,0,38,10),
               #threshold1_wr=c(35,10,32,0,43,24,13,0,33,40),threshold2_wr=c(35,10,32,0,43,24,13,0,33,40))

#params <- list(lookback=55,sdParam=2.5,
               #series=c(1:10),rocCalLength=30,posSizes=rep(1,10),
               #lookback_macd=14,lookback1_macd=5,
               #lookback2_macd=10,
               #threshold=15,nFast=5,
               #nSlow = 29, nSig = 8,
               #lookbackWRL1=c(30,10,15,0,37,40,18,0,15,37),lookbackWRL2=c(30,10,15,0,37,40,18,0,15,37),
               #lookbackS1=c(8,11,8,0,4,8,8,0,8,9), lookbackS2=c(8,11,8,0,4,8,8,0,8,9),
               #lookbackL1=c(45,40,40,0,9,25,10,0,30,12),lookbackL2=c(45,40,40,0,9,25,10,0,30,12),
               #threshold1_wr=c(25,10,30,0,45,30,12,0,30,38),threshold2_wr=c(25,10,30,0,45,30,12,0,30,38))

params <- list(lookback=55,sdParam=2.5,
               series=c(1:10),rocCalLength=30,posSizes=rep(1,10),
               lookback_macd=14,lookback1_macd=5,
               lookback2_macd=10,
               threshold=15,nFast=5,
               nSlow = 29, nSig = 8,
               nWait=rep(1,10),
               lookbackWRL1=c(20,10,10,0,34,30,42,0,37,35),lookbackWRL2=c(30,10,15,0,37,40,18,0,15,37),
               lookbackS1=c(11,10,8,0,5,4,8,0,9,8), lookbackS2=c(8,11,8,0,4,8,8,0,8,9),
               lookbackL1=c(15,42,40,0,15,17,10,0,38,10),lookbackL2=c(45,40,40,0,9,25,10,0,30,12),
               threshold1_wr=c(35,10,32,0,43,24,13,0,33,40),threshold2_wr=c(25,10,30,0,45,30,12,0,30,38),
               lossLimits=c(1,1,0.1,5,5,1,10,2,1,0.1), profitTarget=c(1,1,0.1,5,5,1,10,2,1,0.1))


#params		<- list(spreadPercentage=0.0001,inventoryLimitAs=rep(500,10),series=1:10) # paras for limit strategy
#params <- list(nFast=10, nSlow=22, nSig=8, maType="EMA") #MACD
print("Parameters:")
print(params)

# BACKTEST PARAMETERS ##########################
# split data in two (e.g. for in/out test)
numDays <- nrow(dataList[[1]])
inSampDays <- 1299

# in-sample period
dataList <- lapply(dataList, function(x) x[1:inSampDays])

# out-of-sample period
#dataList <- lapply(dataList, function(x) x[(inSampDays+1):numDays])

sMult <- 0.20 # slippage multiplier

# DO BACKTEST ############################
results <- backtest(dataList,getOrders,params,sMult)
pfolioPnL <- plotResults(dataList,results,plotType='ggplot2')

