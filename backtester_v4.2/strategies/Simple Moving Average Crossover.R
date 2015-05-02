startIndexS <- store$iter - params$lookbackS
startIndexL <- store$iter - params$lookbackL

MAclS <- last(SMA(store$cl[startIndexS:store$iter,i], n=params$lookbackS))
MAclL <- last(SMA(store$cl[startIndexL:store$iter,i], n=params$lookbackL))

#when there's a bullish crossover
MAclS > MAclL
marketOrders[params$series[i]] <- positionSizes[i] # long

#when there's a bearish crossover
MAclS < MAclL
marketOrders[params$series[i]] <- -positionSizes[i]  # short
