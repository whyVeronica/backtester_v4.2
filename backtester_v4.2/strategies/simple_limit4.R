# FOR A GENERAL EXPLANATION OF REQUIREMENTS ON getOrders see rsi.R 

# Marketmaking strategy
# Places buy and sell limit orders around close price
# Spread is determined by daily range
# Unit position sizes for limit orders
# Uses market order to clear inventory when it becomes too large

# Note: limit orders are automatically cancelled at the end of the day

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  marketOrders <- allzero
  limitOrders1 <- allzero
  limitPrices1 <- allzero
  limitOrders2 <- allzero
  limitPrices2 <- allzero
  spread <- allzero

    #cat("currentPos", formatC(currentPos,3),"\n")
  for (i in 1:length(params$series)){
    # check if current inventory is above a limit and if so exit completely
    # with a market order

    #marketOrders <- ifelse(abs(currentPos) > params$inventoryLimits, -currentPos, 0)
    
    # use the range (High-Low) as a indicator for a reasonable "spread" for
    # this pseudo market making strategy
    spread[params$series[i]] <- params$spreadPercentage * (newRowList[[params$series[i]]]$High - newRowList[[params$series[i]]]$Low)
    
    limitOrders1[params$series[i]] <- 1 # BUY LIMIT ORDERS
    limitPrices1[params$series[i]] <- newRowList[[params$series[i]]]$Close - spread[params$series[i]]/2
    
    limitOrders2[params$series[i]] <- -1 # SELL LIMIT ORDERS
    limitPrices2[params$series[i]] <- newRowList[[params$series[i]]]$Close + spread[params$series[i]]/2
  }

    

	return(list(store=store,marketOrders=marketOrders,
	                        limitOrders1=limitOrders1,
	                        limitPrices1=limitPrices1,
	                        limitOrders2=limitOrders2,
	                        limitPrices2=limitPrices2))
}
