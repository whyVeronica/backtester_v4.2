# FOR A GENERAL EXPLANATION OF REQUIREMENTS ON getNewPosList see rsi.R 

# This strategy only trades on certain markets, which is encoded in params$series.

# The strategy will be long (short) on contract whenever the close is below (above) 
# the lower (upper) Bollinger Band of the close.

maxRows <- 1300 # used to initialize a matrix to store closing prices

getNewPosList <- function(store, newRowList, params) {

	if (is.null(store)) 
		store 	<- initStore(newRowList,params$series)
	else 
		store	<- updateStore(store, newRowList, params$series)

	saveStore <<- store
	
	pos <- rep(0,length(newRowList))
	sig <- rep(0,length(newRowList))

    if (store$iter > params$lookback) {
       startIndex <-  store$iter - params$lookback
       #cat(startIndex,store$iter,'\n')
       for (i in 1:length(params$series)) {
           cl 		<- newRowList[[params$series[i]]]$Close
           bbands 	<- BBands(store$cl[startIndex:store$iter,i],n=params$lookback,sd=params$sdParam)

           # determine today's signal

           if (cl < last(bbands[,"dn"])) {
               sig[params$series[i]] <- 1
           }
           else if (cl > last(bbands[,"up"])) {
               sig[params$series[i]] <- -1
           }
           # else sig[params$series[i]] is zero

           # check if we have been in trade too long

           # we maintain that sig[i] is an integer
           # if sig[i] == 0 we were flat last period
           # if sig[i] >  0 we have been long  for sig[i] periods
           # if sig[i] <  0 we have been short for sig[i] periods

           if (sig[params$series[i]] == 1) {# long signal today 

                if (store$count[i] < 0) # last time we were short
                    store$count[i] == sig[params$series[i]] # == 1
                else if (store$count[i] == params$holdPeriod) { # reached holding period
                    sig[params$series[i]] <- 0 # don't stay long
                    store$count[i] <- 0 # reset count to 0
                }
                else # 0 <= store$count[i] != (should be <) params$holdPeriod
                    store$count[i] <- store$count[i] + 1 
           }

           else if (sig[params$series[i]] == -1) {

                if (store$count[i] > 0) # last time we were long
                    store$count[i] == sig[params$series[i]] # == -1
                else if (store$count[i] == -params$holdPeriod) { # reached holding period
                    sig[params$series[i]] <- 0 # don't stay short
                    store$count[i] <- 0 # reset count to 0
                }
                else # 0 >= store$count[i] != (should be >) -params$holdPeriod
                    store$count[i] <- store$count[i] - 1 
           }
           else
                store$count[i] <- 0 # reset count to 0
           }

           pos <- sig

    }
	cat(formatC(store$count,2),'\t',formatC(pos,2),'\n')
	#cat(formatC(pos,2),'\n')
	return(list(store=store,pos=pos))
}

initClStore  <- function(newRowList,series) {
	clStore <- matrix(0,nrow=maxRows,ncol=length(series))
	clStore <- updateClStore(clStore, newRowList, series, iter=1)
    return(clStore)
}

updateClStore <- function(clStore, newRowList, series, iter) {
    for (i in 1:length(series))
        clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
    return(clStore)
}

initStore <- function(newRowList,series) {
	count <- vector(mode="numeric",length=length(series)) # stores # of days in trade
	return(list(iter=1,cl=initClStore(newRowList,series),count=count))
}

updateStore <- function(store, newRowList, series) {
	store$iter 	<- store$iter + 1
	store$cl	<- updateClStore(store$cl,newRowList,series,store$iter) 
	return(store)
}
