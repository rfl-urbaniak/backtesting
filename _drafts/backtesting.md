Backtesting
================

### Set-up

First, load the libraries that will be used in what follows:

``` r
library(quantmod)
library(tidyverse)
library(tidyquant)
library(ggplot2)
library(ggthemes)
library(TTR)
#library(plyr)
#library(ggfortify)
#library(xts)
```

Next, get the most recent data on US500 (because I'll use it for the example), rename the columns for later use, take a peek, check how many days have been recorded, rename the wole dataset as `US500full`, pick the first 2800 days for training and optimization (call it `US500`) and keep the remaining dates for testing as `US500test`.

``` r
getSymbols(Symbols = "^GSPC", src = "yahoo")
```

    ## [1] "^GSPC"

``` r
US500 <- data.frame(GSPC)
colnames(US500) <- c("open","high","low","close","vol","adjusted")
head(US500)
```

    ##               open    high     low   close        vol adjusted
    ## 2007-01-03 1418.03 1429.42 1407.86 1416.60 3429160000  1416.60
    ## 2007-01-04 1416.60 1421.84 1408.43 1418.34 3004460000  1418.34
    ## 2007-01-05 1418.34 1418.34 1405.75 1409.71 2919400000  1409.71
    ## 2007-01-08 1409.26 1414.98 1403.97 1412.84 2763340000  1412.84
    ## 2007-01-09 1412.84 1415.61 1405.42 1412.11 3038380000  1412.11
    ## 2007-01-10 1408.70 1415.99 1405.32 1414.85 2764660000  1414.85

``` r
nrow(US500)
```

    ## [1] 3661

``` r
US500full <- US500
US500 <- US500[1:2800,]
US500test <- US500full[2801:nrow(US500full),]
```

### Adding returns (and being a bit fussy about it)

Normally, to add daily returns one might do so using the closing prices. However, notice that for strategy evaluation this might be misleading, as you are unlikely to be able to buy or sell exactly at the closing price (and you definitely won't be able to use a strategy deciding to sell based on a closing price that do not exist yet, so you can't sell right before the closing). So instead, we'll be using the open prices. As a sanity check, we first do calculate close-based returns just to see that there is a difference later on.

``` r
US500$closeRet <- numeric(nrow(US500))
for(r in 1:(nrow(US500)-1)){
  US500$closeRet[r] <- (US500$close[r+1]/US500$close[r])-1
}

US500$ret <- numeric(nrow(US500))
for(r in 1:nrow(US500)){
  US500$ret[r] <- (US500$open[r+2]/US500$open[r+1])-1
}
US500$ret[(nrow(US500)-1):nrow(US500)] <- rep(0,2)
#this calculates centered returns; will explain and use later:
US500$retC <- US500$ret - mean(US500$ret, na.rm=TRUE)
#now your columns look like this:
head(US500)
```

    ##               open    high     low   close        vol adjusted      closeRet
    ## 2007-01-03 1418.03 1429.42 1407.86 1416.60 3429160000  1416.60  0.0012282861
    ## 2007-01-04 1416.60 1421.84 1408.43 1418.34 3004460000  1418.34 -0.0060845814
    ## 2007-01-05 1418.34 1418.34 1405.75 1409.71 2919400000  1409.71  0.0022203184
    ## 2007-01-08 1409.26 1414.98 1403.97 1412.84 2763340000  1412.84 -0.0005166764
    ## 2007-01-09 1412.84 1415.61 1405.42 1412.11 3038380000  1412.11  0.0019403524
    ## 2007-01-10 1408.70 1415.99 1405.32 1414.85 2764660000  1414.85  0.0063398736
    ##                     ret          retC
    ## 2007-01-03  0.001228286  0.0009328643
    ## 2007-01-04 -0.006401819 -0.0066972410
    ## 2007-01-05  0.002540309  0.0022448873
    ## 2007-01-08 -0.002930279 -0.0032257006
    ## 2007-01-09  0.004358639  0.0040632175
    ## 2007-01-10  0.006346993  0.0060515717

### Building signal

Say you want to try out a fairly straightforward strategy, a variant of Bollinger bands. The idea is to buy if the price enters *m**a*-*m**u**l**t**i**p**l**i**e**r* × *r**u**n**S**D* from below, that is if crosses some multiplier of the running standard deviation down from the moving average. Suppose you're using the same time window to calculate them both. Further, assume you want to sell when the price reaches *m**a*, and that you're only interested in long positions, that is the signal is binary: 1 for holding a long position, 0 for not being on the market. Because we want to reuse the code to define a general function, it will be useful to rename the data as `data` for the time being. Suppose you think 20 days is good and that 1.5 is a decent multiplier.

``` r
#this depends on two parameters
data <- US500
lookback <- 20 #window in days
multiplier <- 1.5  #distance from the mean as measured in running sd

data$ma <- SMA(data$adjusted,lookback) #moving average
data$runSD <- runSD(data$adjusted,lookback) #running sd


data$signal <- numeric(nrow(data))
for (i in (lookback + 2):nrow(data)){
    data$signal[i] <- (
                      #either you're above the threshold today and were below it yesterday
                      data$close[i] >= data$ma[i] - multiplier * data$runSD[i] &
                      data$close[i-1] < data$ma[i] - multiplier * data$runSD[i]) |
                      # or you're holding already, but still below the moving average
                      (data$signal[i-1] == 1 & 
                      data$close[i] < data$ma[i])
    }
```

For later use, it actually makes to define a function that does all this at once.

``` r
signal <- function (data, lookback = 20, multiplier = 1.5) { 
data$ma <- SMA(data$adjusted,lookback)
data$runSD <- runSD(data$adjusted,lookback)
data$signal <- numeric(nrow(data))
for (i in (lookback + 2):nrow(data)){
  data$signal[i] <- (data$close[i] >= data$ma[i] - multiplier * data$runSD[i] &
                     data$close[i-1] < data$ma[i] - multiplier * data$runSD[i]) |
                (data$signal[i-1] == 1 &  data$close[i] < data$ma[i])
}
return(data)
}

#add signal to US500
US500withSignal <- signal(US500)
```

# Don't just annualize expected daily returns

Now, you might think, ok, let's just pick the close-based returns for the days on the market, average out, and calculate what you'd make annually assuming there are 200 working days.

``` r
closeReturns  <- US500withSignal$closeRet[US500withSignal$signal == 1] 
(1+mean(closeReturns))^200
```

    ## [1] 1.229147

Wow, impressive, right? You'd make 22%! Well, first off, the strategy would be expected to make you hold a position only a fraction of the time, namely, 31 days per year on average, so you need to recalculate your annual estimate.

``` r
mean(US500withSignal$signal)
```

    ## [1] 0.1539286

``` r
mean(US500withSignal$signal) * 200
```

    ## [1] 30.78571

``` r
(1+mean(closeReturns))^31
```

    ## [1] 1.032496

This is slightly less impressive, unfortunately. Now, if you use open prices there is going to be a slight difference.

``` r
returns  <- US500withSignal$ret[US500withSignal$signal == 1] 
#annualized
(1+mean(returns))^200
```

    ## [1] 1.228051

``` r
#considering you'd be off the market most of the time
(1+mean(returns))^31
```

    ## [1] 1.032354

Perhaps more concretely, you want to take a look at two equity curves. One for simply buying and holding and one for following your strategy.

``` r
equityHold <- numeric(nrow(US500withSignal))
equityHold[1] <- 1
for (i in 2: nrow(US500withSignal)) {equityHold[i] <-
                                    equityHold[i-1] * (1+ US500withSignal$ret[i])}


returnsFull  <- ifelse(US500withSignal$signal == 1, US500withSignal$ret,0)
equityStrategy <- numeric(nrow(US500withSignal))
equityStrategy[1] <- 1
for (i in 2: nrow(US500withSignal)) {equityStrategy[i] <-
  equityStrategy[i-1] * (1+ returnsFull[i])}

ggplot()+geom_line(aes(x = 1: nrow(US500withSignal), y = equityHold))+
  geom_line(aes(x = 1: nrow(US500withSignal), y = equityStrategy), col = "skyblue")+
  theme_tufte()+xlab("day")+ylab("equity")+ggtitle("Equity curves", subtitle ="hold (black) vs.strategy (blue)")
```

![](https://rfl-urbaniak.github.io/backtesting/images/equityCurves,%20options-1.png)

The result of your strategy is in blue. Note the flat lines: these are the times when your strategy stops you from being on the market. You avoid some drawdowns, but you also avoid making money on some strong positive trends. Crucially, the final equities for the two ways to proceed are:

``` r
tail(equityHold,1)
```

    ## [1] 1.869234

``` r
tail(equityStrategy,1)
```

    ## [1] 1.446046

But perhaps you're not too worried, because you think that you can use your capital on some other markets for the idle days and so in the end your money would we working for you more than 15% of the time. Sure.

### Center your returns!

So suppose what we care about is average daily returns and we're not worried about how many days the strategy tells you to be on the market as long as its a fairly decent exposure time. Here is a problem. When you you looked at average returns, and these can be simply positive because the market went up over the years. The question is, whether your strategy would allow you to beat the market, and to evaluate this better you need to center the returns so that they average to zero and then look at what gain you'd expect from your strategy in terms of the centered returns, including using centered returns in statistical significance estmation (read on for details). So, by how much would you have beaten, expectedly, the market per year if you're only using your resources for `US500`?

``` r
returnsC  <- US500withSignal$retC[US500withSignal$signal == 1] 
(1+mean(returnsC))^31
```

    ## [1] 1.022951

``` r
#as compared to holding the whole year
(1+mean(US500withSignal$retC))^200
```

    ## [1] 1

This, still is a bit of cheating as we repeatedly used the mean return in the calcuations instead of using all the centered returns that were used in the exposure. So, again, let's look at `extra equity lines` which represent how you would stand compared to the market.

``` r
equityHoldC <- numeric(nrow(US500withSignal))
equityHoldC[1] <- 1
for (i in 2: nrow(US500withSignal)) {equityHoldC[i] <-
                                    equityHoldC[i-1] * (1+ US500withSignal$retC[i])}


returnsFullC  <- ifelse(US500withSignal$signal == 1, US500withSignal$retC,0)
equityStrategyC <- numeric(nrow(US500withSignal))
equityStrategyC[1] <- 1
for (i in 2: nrow(US500withSignal)) {equityStrategyC[i] <-
  equityStrategyC[i-1] * (1+ returnsFullC[i])}

ggplot()+geom_line(aes(x = 1: nrow(US500withSignal), y = equityHoldC))+
  geom_line(aes(x = 1: nrow(US500withSignal), y = equityStrategyC), col = "skyblue")+
  theme_tufte()+xlab("day")+ylab("equity")+ggtitle("Extra equity curves", subtitle ="hold (black) vs.strategy (blue)")
```

![](https://rfl-urbaniak.github.io/backtesting/images/equityCurvesTop,%20options-1.png)

``` r
#compare mean daily returns:
mean(returnsC)
```

    ## [1] 0.0007322492

``` r
mean(US500withSignal$retC) #zero with rounding errors
```

    ## [1] 7.857887e-20

Ok. It doesn't seem like you're going to get rich soon, but at least this seems like progress, right? Right?

### Comparison with random strategies

Come to think of it, you only should think your strategy isn't too bad if it does significantly better than just buying and selling on random days for the same number of days.
