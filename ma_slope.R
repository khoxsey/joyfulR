require(quantmod)
#getSymbols('^SPX')
load('demo_data.rda')

spx <- Cl(SPX)
spx$ma <- runMean(Cl(spx), n=200)
for (i in 1:199) {
  spx$ma[i] <- mean(spx$SPX.Close[1:i])
}

spx$rtn <- diff(log(Cl(spx)))
spx$rtn[1] <- 0

#MA slope in points/day
spx$sl10 <- (spx$ma - lag(spx$ma, 10)) / 10 #2 weeks
spx$sl21 <- (spx$ma - lag(spx$ma, 21)) / 21 #1 month
spx$sl63 <- (spx$ma - lag(spx$ma, 63)) / 63 #1 qtr
spx$sl99 <- (spx$ma - lag(spx$ma, 99)) / 99 #half yr-ish

#zero-fill NAs for plotting
spx$sl10[is.na(spx$sl10)] <- 0
spx$sl21[is.na(spx$sl21)] <- 0
spx$sl63[is.na(spx$sl63)] <- 0
spx$sl99[is.na(spx$sl99)] <- 0

spx$sig10 <- lag(sign(spx$sl10))
spx$sig21 <- lag(sign(spx$sl21))
spx$sig63 <- lag(sign(spx$sl63))
spx$sig99 <- lag(sign(spx$sl99))
spx[1, 8:11] <- 0

rg <- '2007-11/2014'
plot(cumsum(spx$sig10[rg]))
lines(cumsum(spx$sig21[rg]), col="blue")
lines(cumsum(spx$sig63[rg]), col="green")
lines(cumsum(spx$sig99[rg]), col="red")

rg <- '2007/2024'
plot(cumsum(spx$rtn * spx$sig10)[rg])
lines(cumsum(spx$rtn * spx$sig21)[rg], col="blue")
lines(cumsum(spx$rtn * spx$sig63)[rg], col="green")
lines(cumsum(spx$rtn * spx$sig99)[rg], col="red")

#Partition off the slower signal
spx$lng99 <- ifelse(spx$sig99 > 0, spx$rtn, 0)
spx$sht99 <- ifelse(spx$sig99 <= 0, spx$rtn, 0)
plot(cumsum(spx$lng99)[rg])
lines(cumsum(spx$sht99)[rg], col="red")
