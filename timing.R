#Simple example market timing
require(quantmod)
#getSymbols('SPY')
load('demo_data.rda')

spy <- cbind(lag(Cl(SPY)), Op(SPY), Cl(SPY))
names(spy) <- c('prevCl','open','close')
spy$prevCl[1] <- spy$open[1] #BS the NA

#Days vs nights
spy$rtnDy <- log(spy$close) - log(spy$open)
spy$rtnNt <- log(spy$open) - log(spy$prevCl)
plot(cumsum(spy$rtnDy))
lines(cumsum(spy$rtnNt))

#Start over
spy <- cbind(lag(Cl(SPY)), Op(SPY), Cl(SPY))
names(spy) <- c('prevCl','open','close')
spy$prevCl[1] <- spy$open[1] #BS the NA

#Odd vs even
spy$jlnOdd <- ifelse(julian(index(spy)) %% 2 == 1, 1,0)
spy$dayOdd <- ifelse(as.numeric(format(index(spy), "%d")) %% 2 == 1, 1, 0)
