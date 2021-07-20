---
title:
layout: page
toc: true
#title: Bayesian Networks for the Legal Probabilism SEP entry
output:
  md_document:
    variant: markdown_github
    preserve_yaml: true
---




Suppose you heard easy money can be made by online trading. You want to give it a shot,  so you open an account with an online broker. You google a bit and apparently one way to go about trading is to define a strategy telling you when to buy and when to sell,  "backtest" it (that is, see how it would have performed on past data), perhaps tweaking it a bit in the process, and to go ahead once you're more or less satisfied. That's the way to beat the market. Or so they say...

Here's on one hand an explanation of how you could formulate and "backtest" your trading strategy using **R**, and on the other, why it might not be worth your time, as backtesting is a flawed methodology. I also show you what methodology is better (hint: it uses Monte Carlo methods to estimate the chance a random strategy would to as good as your shiny source of pride), and why you might be disappointed with the result.

- While I am aware some of the moves could be made more efficient and code shorter, I do certain things in a somewhat lengthy manner for the sake of accessibility.

- For a sensible criticism of subjective technical analysis and a nice treatment of some of the conceptual points I'm going to discuss, I highly recommend [Evidence-Based Technical Analysis](https://www.amazon.com/Evidence-Based-Technical-Analysis-Scientific-Statistical/dp/0470008741) by David Aronson.


**Contents**
* TOC
{:toc}



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




```{r data, options, warning = FALSE, message = FALSE}
getSymbols(Symbols = "^GSPC", src = "yahoo")
US500 <- data.frame(GSPC)
colnames(US500) <- c("open","high","low","close","vol","adjusted")
head(US500)
nrow(US500)
US500full <- US500
US500 <- US500[1:2800,]
US500test <- US500full[2801:nrow(US500full),]
```

``` r
getSymbols(Symbols = "^GSPC", src = "yahoo")
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

Say you want to try out a fairly straightforward strategy, a variant of Bollinger bands. The idea is to buy if the price enters  $ma$-$multiplier\times runSD$ from below, that is if crosses  some multiplier of the running standard deviation down from the moving average. Suppose you're using the same time window to calculate them both. Further, assume you want to sell when the price reaches $ma$, and that you're only interested in long positions, that is the signal is binary: 1 for holding a long position, 0 for not being on the market.  Because we want to reuse the code to define a general function, it will be useful to rename the data as `data` for the time being.
Suppose you think 20 days is good and that 1.5 is a decent multiplier.
