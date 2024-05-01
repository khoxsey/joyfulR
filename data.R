# S&P 500 Index data broadly available
# Rstudio/gt here: https://github.com/rstudio/gt/tree/master/data
#
# The ever-reliable Yahoo Finance has everything since the GFC:
require(quantmod)
getSymbols('^SPX')
getSymbols('SPY')

save(SPX,SPY, file='./demo_data.rda')
