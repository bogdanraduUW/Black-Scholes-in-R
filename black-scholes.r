library(quantmod)
library(dplyr)
library(tidyquant)

black_scholes <- function(S, K, T, r, sigma, type = "call") {
    d1 <- (log(S/K) + (r + sigma^2/2) * T) / (sigma * sqrt(T))
    d2 <- d1 - sigma * sqrt(T)

    if(type == "call") {
        option_price <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
    } else if(type == "put") {
        option_price <- K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
    } else {
        stop("Invalid option type. Please use 'call' or 'put'.")
    }
    
    return(option_price)
}

ticker <- "AAPL"
start_date <- as.Date("2022-01-01")
end_date <- as.Date("2023-01-01")

stock_data <- getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
stock_prices <- stock_data %>% Ad() %>% `colnames<-`(ticker)

historical_volatility <- function(prices, n = 252) {
    daily_returns <- dailyReturn(prices, type = "log")
    return(sd(daily_returns) * sqrt(n))
}

volatility <- historical_volatility(stock_prices)

S <- as.numeric(last(stock_prices)) # Current stock price
K <- S * 1.1 # Strike price (10% above current stock price)
T <- 30/365 # Time to expiration (30 days)
r <- 0.02 # Risk-free rate (2%)

call_option_price <- black_scholes(S, K, T, r, volatility, type = "call")
put_option_price <- black_scholes(S, K, T, r, volatility, type = "put")

cat("Call option price: ", call_option_price, "\n")
cat("Put option price: ", put_option_price, "\n")