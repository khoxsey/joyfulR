# Testing the old trope about the 200dma
require(quantmod)
#getSymbols('^SPX')
load('demo_data.rda')

spx <- Cl(SPX)
spx$ma <- runMean(Cl(spx), n=200)
for (i in 1:199) {
  spx$ma[i] <- mean(spx$SPX.Close[1:i])
}
spx$disp <- Cl(spx) - spx$ma

#Get your lags right
# spx$jnk <- sign(spx$disp)
# spx['2023-10']
# spx$jnk <- NULL

#The right way
spx$sig <- sign(lag(spx$disp))
spx$sig[1] <- 0

# Rtns above/below dma
spx$rtn <- diff(log(Cl(spx)))
spx$rtn[1] <- 0
spx$lev <- spx$rtn * spx$sig
plot(cumsum(spx$lev))
lines(cumsum(spx$rtn), col="green")

#Partition returns above/below MA
spx$abv <- ifelse(spx$sig>0, spx$rtn, 0)
spx$blw <- ifelse(spx$sig<0, spx$rtn, 0)
plot(cumsum(spx$abv))
lines(cumsum(spx$blw), col="red") #NB: positive returns during short signal!
lines(cumsum(spx$rtn), col="green")
