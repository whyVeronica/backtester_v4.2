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
#strategyFile <-'strategies/Latest/Williams%R.R'
#strategyFile <-'strategies/Latest/Williams%R & MA.R'
#strategyFile <-'strategies/Latest/MACD(posSizes-mean abs diff).R'

#strategyFile <-'strategies/Latest/W%R-MACD-MA1-stoploss.R'
#strategyFile <-'strategies/Latest/W%R-MACD-MA2-Crossover-stoploss.R'
#strategyFile <-'strategies/Latest/W%R-MACD-MA3-Crossover-stoploss-targetProfit.R'
#strategyFile <-'strategies/Latest/W%R-MACD-MA4-stoploss-profitTarget.R'
#strategyFile <-'strategies/Latest/W%R-MACD-MA5-Crossover-profitPos.R'
#strategyFile <-'strategies/Latest/W%R-MACD-MA6-Crossover-Original-limit.R'
strategyFile <-'strategies/Latest/W%R-MACD-MA3-limit1.R'

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

params <- list(lookbackWRL=10,lookbackS=5,lookbackL=10,threshold=25,
              lossLimits=1000, profitTarget=3000, series=1)#W%R & macd 

#params <- list(lookbackR=14,lookbackS=5,lookbackL=30,lookbackLimit=10,
               #threshold=40,nFast=5, nSlow=30, nSig=5,
               #lossLimits=5000, profitTarget=5000, series=c(1:10))#W%R & macd 

print("Parameters:")
print(params)

# BACKTEST PARAMETERS ##########################
# split data in two (e.g. for in/out test)
numDays <- nrow(dataList[[1]])
inSampDays <- 50
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
