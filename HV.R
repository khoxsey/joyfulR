require(quantmod)
load('demo_data.rda')

spx <- Cl(SPX)
spx$rtn <- diff(log(Cl(SPX)))
spx$rtn[1] <- 0

#Rtns over several windows
spx$rtn10 <- runSum(spx$rtn, n=10)
spx$rtn21 <- runSum(spx$rtn, n=21)
spx$rtn63 <- runSum(spx$rtn, n=63)

#HV over several lookback periods, annualized
spx$hv10 <- runSD(spx$rtn, n=10) * sqrt(252)
spx$hv21 <- runSD(spx$rtn, n=21) * sqrt(252)
spx$hv63 <- runSD(spx$rtn, n=63) * sqrt(252)

#Does k-day-old HV correlate with today's returns?
par(mfrow=c(3,3))
plot(coredata(lag(spx$hv10, k=10)), coredata(spx$rtn10))
plot(coredata(lag(spx$hv10, k=10)), coredata(spx$rtn21))
plot(coredata(lag(spx$hv10, k=10)), coredata(spx$rtn63))
plot(coredata(lag(spx$hv21, k=21)), coredata(spx$rtn10))
plot(coredata(lag(spx$hv21, k=21)), coredata(spx$rtn21))
plot(coredata(lag(spx$hv21, k=21)), coredata(spx$rtn63))
plot(coredata(lag(spx$hv63, k=63)), coredata(spx$rtn10))
plot(coredata(lag(spx$hv63, k=63)), coredata(spx$rtn21))
plot(coredata(lag(spx$hv63, k=63)), coredata(spx$rtn63))
par(mfrow=c(1,1))
#Nope

#Add a day of lag, just to be sure
par(mfrow=c(3,3))
plot(coredata(lag(spx$hv10, k=11)), coredata(spx$rtn10))
plot(coredata(lag(spx$hv10, k=11)), coredata(spx$rtn21))
plot(coredata(lag(spx$hv10, k=11)), coredata(spx$rtn63))
plot(coredata(lag(spx$hv21, k=22)), coredata(spx$rtn10))
plot(coredata(lag(spx$hv21, k=22)), coredata(spx$rtn21))
plot(coredata(lag(spx$hv21, k=22)), coredata(spx$rtn63))
plot(coredata(lag(spx$hv63, k=64)), coredata(spx$rtn10))
plot(coredata(lag(spx$hv63, k=64)), coredata(spx$rtn21))
plot(coredata(lag(spx$hv63, k=64)), coredata(spx$rtn63))
par(mfrow=c(1,1))
#Nope

#Slope of 21:63 HV sort of like vix slope?
spx$sl <- spx$hv63 - spx$hv21
spx$sl[is.na(spx$sl)] <- 0
plot(coredata(spx$sl), coredata(spx$rtn21))

# Slope and returns seem coincident
par(mfrow=c(2,1))
plot(spx$sl)
plot(spx$rtn21)
par(mfrow=c(1,1))

#Most basic signal
spx$sig <- lag(sign(spx$sl))
spx$sig[1] <- 0
plot(cumsum(spx$rtn * spx$sig))

#Looks like the signal stops working in 2013
plot(cumsum(spx$rtn), col="green", lwd=2)
lines(cumsum(spx$sig * spx$rtn), col="black", lwd=2)
lines(cumsum(ifelse(spx$sig > 0, spx$rtn, 0)), col="blue", lwd=2)
lines(cumsum(ifelse(spx$sig < 0, -1*spx$rtn, 0)), col="red", lwd=2)

#Thoughtful pause...
plot(cumsum(spx$rtn * spx$sig), lwd=2)
lines(cumsum(spx$rtn * sign(spx$sl)), col="red", lwd=2)
#Interesting avenue for further investigation:
# - pay for 1m bars of ^SPX, /SP or /ES outrights, maybe SPY as well
# - use the 1m bars to calculate turnover points in the 24-hr market
# - test whether front-running the signal is worthwhile
#
# Trade expression might use /ES for entry outside of cash hours, and
# options on SPX or SPY for positioning
