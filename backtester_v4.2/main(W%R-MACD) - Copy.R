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
#strategyFile <-'strategies/Latest/Williams%R.R'
#strategyFile <-'strategies/Latest/Williams%R & MA.R'
#strategyFile <-'strategies/Latest/MACD(posSizes-mean abs diff).R'

#strategyFile <-'strategies/Latest/W%R-MACD-MA1-stoploss.R'
#strategyFile <-'strategies/Latest/W%R-MACD-MA2-Crossover-stoploss.R'
#strategyFile <-'strategies/Latest/W%R-MACD-MA-Crossover-stoploss-targetProfit1.R'
#strategyFile <-'strategies/Latest/W%R-MACD-MA4-stoploss-profitTarget.R'
#strategyFile <-'strategies/Latest/W%R-MACD-MA5-Crossover-profitPos.R'
#strategyFile <-'strategies/Latest/W%R-MACD-MA6-Crossover-Original-limit2.R'
#strategyFile <-'strategies/Latest/W%R-MACD-MA3-limit4.R'
#strategyFile <-'strategies/Latest/MACD-MA3-limit1.R'
#strategyFile <-'strategies/Latest/5.R'
#strategyFile <-'strategies/Modify/sha.R'
#strategyFile <-'strategies/Modify/wait1.R'
strategyFile <-'strategies/Modify/limit1.R'
#strategyFile <-'strategies/simple_limit4.R'
###############################
# Strategies using Limit orders
###############################
#strategyFile <-'strategies/simple_limit2.R'

cat("Sourcing",strategyFile,"\n")
source(strategyFile) # load in getOrders

# STRATEGY PARAMETERS ###########################
#params <- list(nFast=5, nSlow=10, nSig=5,nWait=0,series=c(1:10))#MACD

#params <- list(lookbackR=14,lookbackS=5,lookbackL=8,threshold=30,
               #series=c(1:10), #W%R & MA 

#params <- list(lookback=14,lookback1=5,lookback1=8,threshold=30,series=c(1:10))#W%R & MA 

#params <- list(lookbackWRL2=40,lookbackS2=5,lookbackL2=16,threshold2=20,
              #lossLimits2=1000, profitTarget2=1000, 
              #lookbackWRL1=40,lookbackS1=5,lookbackL1=25,threshold1=20,
              #lossLimits1=5000, profitTarget1=5000,
              #series=6)#W%R & macd 

#params  	<- list(lookbackLimit=c(48,67,103,136),series=c(1,6,7,8)) # params for limit strategy

params    <- list(lookbackLimit=c(67,136),series=c(6,8)) # params for limit strategy
#params <- list(lookbackR=10,lookbackS=5,lookbackL=10,threshold=30,nWait=2,
               #lossLimits=5, profitTarget=5,lookbackLimit=10,series=2)#W%R & macd 

#params <- list(lookbackR=14,lookbackS=5,lookbackL=10,lookbackLimit=10,
               #threshold=30,nFast=5, nSlow=20, nSig=5,
               #lossLimits=5000, profitTarget=5000, series=c(1:10))#W%R & macd 

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

sMult <- 0.2 # slippage multiplier

# DO BACKTEST ############################
results <- backtest(dataList,getOrders,params,sMult)
pfolioPnL <- plotResults(dataList,results,plotType='ggplot2')

# create a pdf image file
#dev.copy(pdf, file="images/WR-MACD_14-26-12-26-9-3-c(1,7)-clearInv100-P&R-out.pdf")
#dev.off()
