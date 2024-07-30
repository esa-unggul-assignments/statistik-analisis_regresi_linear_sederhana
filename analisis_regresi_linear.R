library(readr)
library(dplyr)

data <- read_csv("DaftarSaham100.csv", col_types = cols(
  No = col_double(),
  Code = col_character(),
  Name = col_character(),
  LastPrice = col_double(),
  MarketCap = col_double()
))

data <- data[, c("LastPrice", "MarketCap")]

data$LastPrice <- as.numeric(data$LastPrice)
data$MarketCap <- as.numeric(data$MarketCap)
data <- na.omit(data)

mean_LastPrice <- mean(data$LastPrice)
mean_MarketCap <- mean(data$MarketCap)

covariance <- sum((data$MarketCap - mean_MarketCap) * (data$LastPrice - mean_LastPrice))
variance <- sum((data$MarketCap - mean_MarketCap)^2)

slope <- covariance / variance

intercept <- mean_LastPrice - slope * mean_MarketCap

regression_equation <- paste("LastPrice =", round(intercept, 2), "+", round(slope, 2), "* MarketCap")

print(list(
  Mean_LastPrice = mean_LastPrice,
  Mean_MarketCap = mean_MarketCap,
  Slope = slope,
  Intercept = intercept,
  Regression_Equation = regression_equation
))
