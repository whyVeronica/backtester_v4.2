####################################################################################
# Profit Drawdown ratio 
# (variant of the Calmar ratio using profit/loss)

maxdrawdown <- function (cumPnL) {
        as.numeric(max(cummax(cumPnL)-cumPnL))
}
maxdrawdown.end <- function (cumPnL) {
        index(cumPnL)[which.max(cummax(cumPnL)-cumPnL)]
}
maxdrawdown.start <- function (cumPnL) {
        index(cumPnL)[which.max(cumPnL[1:which.max(cummax(cumPnL)-cumPnL)])]
}
pdratio <- function(cumPnL) {
    if (last(cumPnL) > 0)
        ret <- last(cumPnL)/maxdrawdown(cumPnL)
    else 
        ret <- last(cumPnL)
    return(as.numeric(round(ret,2)))
}
pdratioString <- function(cumPnL) {
    mdd <- maxdrawdown(cumPnL) # >= 0
    pdr <- pdratio(cumPnL) 
    if (last(cumPnL) > 0)
        return(paste("PD ratio = ", round(last(cumPnL),2), "/", round(mdd,2), "=", pdr)) 
    else 
        return(paste("PD ratio = ", round(last(cumPnL),2))) 
}
