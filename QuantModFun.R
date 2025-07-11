# SMAC group code

(today <- Sys.Date())
(three_months_ago <- seq(today, length = 2, by = "-3 months")[2])
#install.packages("quantmod")


getSymbols("AAPL", from = three_months_ago, to = today)

library(quantmod)
getSymbols("AAPL", from = three_months_ago, to = today)
candleChart(AAPL, theme = 'white', type = 'candles')

AAPL_returns <- na.omit(ClCl(AAPL))
mean(AAPL_returns)
median(AAPL_returns)
sd_AAPL = sd(AAPL_returns) #higher sd = more risk
sd_AAPL
## Excess Kurtosis
mu <- mean(AAPL_returns)
(k <- mean((AAPL_returns - mu)^4)/(mean((AAPL_returns - mu)^2))^2 - 3)

## Daily return plots
plot(AAPL_returns, main = "Apple Daily Log Returna")
abline(h = 0, col = "red") # looking at any extreme events

## return dist. plot and the skewness
hist(AAPL_returns, breaks = 50, main = "Return Distribution", col = 'pink')
#install.packages("moments")
library(moments)
skewness(AAPL_returns) # trying to see if the return are more weighted on a side

# Comparing with the market benchmarks
getSymbols("SPY", from = three_months_ago, to = today)
SPY_returns = na.omit(ClCl(SPY))
cor(AAPL_returns, SPY_returns) ## we know that if we get a high correlation, apple moves with the market

# The sharpe ratio
sharpe_ratio <- mean(AAPL_returns) / sd(AAPL_returns)
sharpe_ratio # now we see how much return based on unit of risk

# cumulative returns
# want to see long term growth from compouding daily returns

cumm_returns <- cumprod(1+ AAPL_returns) -1
plot(cumm_returns, main = "AAPL Cumulative Reutrns", col="lightblue")



# exploring concepts introduce in Paul introduces quant fin book :)
# section 1.2 - dividens and section 1.5 - indices
# looking at equity price data, stock quotes, indices, and price series
head(AAPL)
chartSeries(AAPL, theme = chartTheme("white")) # how traders, analysts, finance bros visualize stock prices over time

Cl(AAPL) # closing price
Hi(AAPL) # high
Vo(AAPL) # volume
# features of market data that reflect market activity

# moving averages and trend evaluation
chartSeries(AAPL)
addSMA(n = 10) # 10 day simple moving average
addSMA(n = 30) # 30 dat simple moving average

# time period
AAPL['2024'] # all 2024
AAPL['2025-01'] # jan 2025
AAPL["2025-01/2025-03"]  # quarter 1 2025

# Lag and differences

lag(Cl(AAPL), k = 1) # yesterday's close
diff(Cl(AAPL), lag = 1) # price change