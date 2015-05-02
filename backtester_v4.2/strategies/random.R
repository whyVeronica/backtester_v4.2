# This strategy uses only marker orders

# Every day this strategy trades a random size 
# marketOrder between -maxLots and +maxLots 

# The random numbers are chosen independently 
# for each series (market)

getOrders <- function(store, newRowList, currentPos, params) {

    allzero  <- rep(0,length(newRowList)) 

	m <- params$maxLots 
	n <- length(newRowList)
	marketOrders=sample(-m:m,size=n,replace=TRUE)

	return(list(store=store,marketOrders=marketOrders,
	                        limitOrders1=allzero,
	                        limitPrices1=allzero,
	                        limitOrders2=allzero,
	                        limitPrices2=allzero))
}
