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


## July 15th 2025

## Building and Evaluating Predicitve Models in R for Financial data:
#(machinelearningmastery.com / using r for predictive modeling in finance)
#install.packages("ggplot2")
#install.packages("dplyr")
library(ggplot2)
library(dplyr)

# simulating financial data:
set.seed(73)
n <- 100
data = data.frame(
  Date = seq.Date(from = as.Date("2025-07-15"), by = "month", length.out = n),
  Revenue = cumsum(rnorm(n, mean = 100, sd = 10)),
  Expense = cumsum(rnorm(n, mean = 50, sd = 5))
)
head(data)

## feature engineering, making lagged features for prediction, splitting data into training and test
# creating lagged feats
data <- data %>% 
  arrange(Date) %>%
  mutate(Lag1_Revenue = lag(Revenue, 1),
         Lag2_Revenue = lag(Revenue, 2),
         Lag1_Expense = lag(Expense, 1),
         Lag2_Expense = lag(Expense, 2)) %>%
  na.omit()

# split data
set.seed(73)
train_index <- sample(seq_len(nrow(data)), size = 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Traing the linear regression model to predict financial metric based on lagged features
model <- lm(Revenue ~ Lag1_Revenue + Lag2_Revenue + Lag1_Expense + Lag2_Expense, data = train_data)

# Making predictions that will help in evaluating model performance
predictions <- predict(model, newdata = test_data)

# Evaluating the model yay!
results <- data.frame(Actual = test_data$Revenue, Predicted = predictions)
rmse <- sqrt(mean((results$Actual - results$Predicted)^2))
cat("root mean squared error (rmse):", rmse, "\n")

# Visualize
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "limegreen") +
  labs(title = "Actual vs Predicted Revenue",
       x = "Actual Revenue",
       y = "Predicted Revenue") +
  theme_minimal()

# Practical Application: Stock Price Prediction B)

# past data
getSymbols("AAPL", from = "2022-01-01", to = Sys.Date())
aapl_price_data <- data.frame(Date = index(AAPL), Close = as.numeric(Cl(AAPL)))

# lag features
aapl_price_data <- aapl_price_data %>%
  arrange(Date) %>%
  mutate(
    Lag1_Close = lag(Close, 1),
    Lag2_Close = lag(Close, 2)
  ) %>%
  na.omit()

#train test split 80/20
set.seed(73)
aapl_train_size <- floor(0.8 * nrow(aapl_price_data))
aapl_train_data <- aapl_price_data[1:aapl_train_size, ]
aapl_test_data <- aapl_price_data[(aapl_train_size + 1):nrow(aapl_price_data), ]

# model 
aapl_model <- lm(Close ~ Lag1_Close + Lag2_Close, data = aapl_train_data)

# prediction
aapl_predictions <- predict(aapl_model, newdata = aapl_test_data)

# eval model performance
aapl_results <- data.frame(
  Date = aapl_test_data$Date,
  Actual_Close = aapl_test_data$Close,
  Predicted_Close = aapl_predictions
)

aapl_rmse <- sqrt(mean((aapl_results$Actual_Close - aapl_results$Predicted_Close)^2))
cat("AAPL Prediction rmse:", aapl_rmse, "\n") #pretty decent

# visuals
ggplot(aapl_results, aes(x = Date)) +
  geom_line(aes(y = Actual_Close, color = "Actual")) +
  geom_line(aes(y = Predicted_Close, color = "Predicted")) +
  labs(title = "AAPL Closing Price Prediction",
       y = "Price (USD)", color = "Legend") +
  theme_minimal()

# a very basic buy/sell signal
aapl_results <- aapl_results %>%
  mutate(Signal = ifelse(Predicted_Close > lag(Actual_Close), "Buy", "Sell"))

head(aapl_results[, c("Date", "Actual_Close", "Predicted_Close", "Signal")], 10)
