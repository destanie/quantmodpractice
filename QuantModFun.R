(today <- Sys.Date())
(three_months_ago <- seq(today, length = 2, by = "-3 months")[2])
install.packages("quantmod")


getSymbols("AAPL", from = three_months_ago, to = today)

library(quantmod)
getSymbols("AAPL", from = three_months_ago, to = today)
candleChart(AAPL, theme = 'white', type = 'candles')

AAPL_returns <- na.omit(ClCl(AAPL))
mean(AAPL_returns)
median(AAPL_returns)

## Excess Kurtosis
mu <- mean(AAPL_returns)
(k <- mean((AAPL_returns - mu)^4)/(mean((AAPL_returns - mu)^2))^2 - 3)
