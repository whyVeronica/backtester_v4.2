####################################################################################
# STRATEGY CODE
#
# getOrders is the interface between the backtester and your strategies
####################################################################################

# It's arguments and return list should stay the same.

####################################################################################
# ARGUMENTS 
####################################################################################

# store: see explanation under returned list below

# newRowList: this is a list of single-row OHLCV xts objects, one for each series

# currentPos: this is the current position (so if one wants to close all
# positions one could set marketOrders as -currentPos)

# params: these are the strategy params

####################################################################################
# RETURNED LIST
####################################################################################

# This function must return a list with the the following named memebers

# store
# marketOrders
# limitOrders1
# limitPrices1
# limitOrders2
# limitPrices2

# store contains data required by the strategy is in the variable store
# store is returned by the function and passed back to it the next time 
# it is called by the backetester.

# marketOrders is a vector containing the number of lots to be traded at the 
# open for each series that the
# strategy will hold on the current day
#
# The i'th entry of the integer vector pos (of length nseries) represents 
# the number of contract you want to be long (short) on the next trading day if 
# the entry is positive (negative).

# Entries of marketOrders, limitOrders1, and limitOrders2 should be integers 
####################################################################################

# This strategy uses only market orders

# The strategy will go short if RSI is > 50 + threshold 
#                      long  if RSI is < 50 - threshold

# Set this as number of rows in data (can be larger but should not be smaller)
maxrows <- 1100

getOrders <- function(store, newRowList, currentPos, params) {

	if (is.null(store)) {
		checkParams(params)
		store <- initStore(newRowList)
	}
	else 
		store <- updateStore(store, newRowList)
	
    marketOrders <- rep(0,length(newRowList))
    limitOrders1 <- rep(0,length(newRowList))
    limitPrices1 <- rep(0,length(newRowList))
    limitOrders2 <- rep(0,length(newRowList))
    limitPrices2 <- rep(0,length(newRowList))

	if (store$iter > params$lookback) {
		marketOrders   <- sapply(1:length(newRowList),
		                             function(x) lgStFt(store$cl,x,store$iter))
	}

    # exit positions from yesterday
    marketOrders <- marketOrders - currentPos 

	return(list(store=store,marketOrders=marketOrders,
	                        limitOrders1=limitOrders1,
	                        limitPrices1=limitPrices1,
	                        limitOrders2=limitOrders2,
	                        limitPrices2=limitPrices2))
}

checkParams <- function(params) { # make sure params are correct
    if (!"lookback" %in% names(params))
        stop("Parameter lookback not defined for strategy RSI")
    if (!"threshold" %in% names(params))
        stop("Parameter lookback not defined for strategy RSI")
    if (params$threshold < 0 || params$threshold > 50)
        stop("Parameter lookback is not between 0 and 50")
}

####################################################################################
# All the subsequent functions were designed to simplify and 
# improve the readaility of getNewPos(); 
#
# While not required, this type of function-based approach is advisable 
# and these functions will likely have analogues in your strategies
####################################################################################

initClStore  <- function(newRowList) {
	clStore <- matrix(0,nrow=maxrows,ncol=length(newRowList))
	clStore <- updateClStore(clStore, newRowList, iter=1)
    return(clStore)
}

updateClStore <- function(clStore, newRowList, iter) {
    for (i in 1:length(newRowList))
        clStore[iter,i] <- as.numeric(newRowList[[i]]$Close)
    return(clStore)
}

initStore <- function(newRowList) {
	return(list(iter=1,cl=initClStore(newRowList)))
}

updateStore <- function(store, newRowList) {
	store$iter 	<- store$iter + 1
	store$cl	<- updateClStore(store$cl,newRowList,store$iter) 
	return(store)
}

lgStFt <-	function(clStore,column,iter) {
	startIndex <- iter - params$lookback - 1
	rsi <- last(RSI(clStore[startIndex:iter,column],n=params$lookback)) 
	
	if (rsi > (50 + params$threshold))
        return(-1) # short
	if (rsi < (50 - params$threshold))
        return(1)  # long
    return(0)
}
