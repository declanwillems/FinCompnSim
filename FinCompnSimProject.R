# Financial Computation and Simulation
# Project 2025
# Declan Willems & Jacob Steen

# File for cleaning data

rm(list = ls())

library("plotly")
# Package to connect points in scatter plot to make a surface
# Utilized in part 1 for IV plot
library("akima")
library("dplyr")

setwd("C:\\Users\\willed3\\OneDrive - Rensselaer Polytechnic Institute\\Documents\\Spring 2025\\Financial Computation & Simulation\\Project\\Data")


stock_data <- read.csv("act_stocks.csv")

option_data <- read.csv("act_options.csv")


unique_tickers <- unique(stock_data$Ticker)

print(unique_tickers)


# For this analysis, we are going to pick 1 stock from each of the following sectors

# Tech: Apple, Ticker: AAPL
# Energy: American Electric Power, Ticker: AEP
# Consumer: American Eagle, Ticker: AEO
# Consulting: Accenture, Ticker: ACN
# Insurance: Aflac, Ticker: AFL
# Food / Ags. Commodities: Archer-Daniels-Midland Company, Ticker: ADM

ticker_choices <- c("AAPL", "AEP", "AEO", "ACN", "AFL", "ADM")


stock_filtered <- stock_data[stock_data$Ticker %in% ticker_choices, ]

options_filtered <- option_data[option_data$Stock.Ticker %in% ticker_choices, ]


stock_filtered <- na.omit(stock_filtered)
options_filtered <- na.omit(options_filtered)


ticker_calibration <- function(tickers, stock_data){
  
  ticker_data <- stock_data %>%
                filter(Ticker == tickers) %>%
                arrange(Date)
  
  
  ticker_data$Log.Return <- c(NA, diff(log(ticker_data$Close)))
  
  ticker_data <- na.omit(ticker_data)
  
  mu_daily <- mean(ticker_data$Log.Return)
  
  sigma_daily <- sd(ticker_data$Log.Return)
  
  mu_annual <- 252 * mu_daily
  sigma_annual <- sqrt(252) * sigma_daily
  
  
  return(data.frame(
    
    Ticker = tickers,
    mu_daily = mu_daily,
    mu_annual = mu_annual,
    sigma_daily = sigma_daily,
    sigma_annual = sigma_annual
  ))
  
}


# Loop through for all tickers to calibrate

calibrated_values <- do.call(rbind, lapply(ticker_choices, function(ticker) {
  
  ticker_calibration(ticker, stock_filtered)
}))

head(calibrated_values)

# Implied Volatility Surfaces from observed option prices for all stocks

stock_filtered$Date <- as.Date(stock_filtered$Date)

options_filtered$Trade.Date <- as.Date(options_filtered$Trade.Date)

options_filtered$Expiry.Date <- as.Date(options_filtered$Expiry.Date)

options_filtered$TTM <- as.numeric(difftime(options_filtered$Expiry.Date, options_filtered$Trade.Date, units = "days")) / 365


plot_IV_surface <- function(options_data, tickers){
  
  
  for (ticker in tickers){
    
    iter_data <- subset(options_data, Stock.Ticker == ticker)
    
    
    
    interpolate_grid <- akima::interp(
      
      x = iter_data$Strike,
      y = iter_data$TTM,
      z = iter_data$Avg.IV,
      duplicate = "mean", # if multiple points are at same point
      linear = TRUE
    )
    
    plot_inputs <- plot_ly(
      
      x = interpolate_grid$x,
      y = interpolate_grid$y,
      z = interpolate_grid$z,
      type = "surface"
      
    ) %>%
      layout(
        title = paste("IV Surface:", ticker),
        scene = list(
        xaxis = list(title = "Strike"),
        yaxis = list(title = "Time to Maturity"),
        zaxis = list(title = "Avg. IV")
        )
      )
    print(plot_inputs)
  }
}


plot_IVs <- plot_IV_surface(options_filtered, ticker_choices)



# Model to back out the prices of the options data
# Black-Scholes Model
# Need another model for comparison with American option pricing
option_price_backout <- function(stock_data, options_data, r){
  
  # Black-Scholes model to back out price from options data
  
  
  
  # Merge stock data and options data inside loop as to get S0 to be the close price
  # on the given options trade date
  
  merge_datasets <- left_join(options_data, stock_data,
                              by = c("Trade.Date" = "Date", "Stock.Ticker" = "Ticker"))
  
  S0 <- merge_datasets$Close
  
  K <- merge_datasets$Strike
  
  t <- as.Date(merge_datasets$Trade.Date)
  T <- as.Date(merge_datasets$Expiry.Date)
  
  sigma <- merge_datasets$Avg.IV
  
  # Calculate yearly time diff
  time_diff <- as.numeric(T - t) / 365
  
  d1 <- (1 / (sigma * sqrt(time_diff))) * (log(S0 / K) + (r + 0.5 * sigma^2) * (time_diff))
  
  d2 <- d1 - sigma * sqrt(time_diff)
  
  options_data$Call_Price <- S0 * pnorm(d1) - K * exp(-r * time_diff) * pnorm(d2)
  
  return(options_data)
  
}

rf <- 0.045

call_price_test <- option_price_backout(stock_filtered, options_filtered, rf)


# Build pricing model for American options
# Above function only works for Euro options as per Black-Scholes


binom_lattice_pricing <- function(stock_data, option_data, rf){
  
  
  
  
}




# Part 2
# Build GBM Model

GBM_sim <- function(S0, mu, r, t, sigma, N, M){
  
  
  dt <- t / N
  
  time_vec <- seq(0, t, length = N + 1)
  
  stock_paths <- matrix(0, nrow = N + 1, ncol = M)
  
  stock_paths[1, ] <- S0
  
  set.seed(1730)
  
  for (i in 2:(N + 1)){
    
    
    Z <- rnorm(M)
    
    stock_paths[i, ] <- stock_paths[i - 1, ] * exp((mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z)
  }
  return(list(time = time_vec, stock_paths = stock_paths))
}


S0 <- 50
mu <- 0.05
r <- 0.05
t <- 1
sigma <- 0.3
N <- 252
M <- 100

GBM_paths <- GBM_sim(S0, mu, r, t, sigma, N, M)


Jump_diff <- function(S0, mu, r, t, sigma, N, M, a, b, lambda){
  
  #x_gbm <- x_jd <- matrix(NA, ncol = N + 1, nrow = M)
  
  x_jd <- matrix(NA, ncol = N + 1, nrow = M)
  
  #x_gbm[, 1] <- log(S0)
  
  x_jd[, 1] <- log(S0)
  
  dt <- t / N
  
  sqdt <- sqrt(dt)
  
  set.seed(1730)
  
  for (i in 1:N){
    
    
    Z <- matrix(rnorm(M), ncol = 1)
    
    # Poisson for jump diff.
    NN <- matrix(rpois(M, lambda * dt), ncol = 1)
    
    
    Z_2 <- matrix(rnorm(M), ncol = 1)
    
    MM <- a * NN + b * sqrt(NN) * Z_2
    
    x_jd[, i + 1] <- x_jd[, i] + (mu - 0.5 * sigma^2) * dt + sigma * sqdt * Z + MM
    
    #x_gbm[, i + 1] <- x_gbm[, i] + (mu - 0.5 * sigma^2) * dt + sigma * sqdt * z
    
    
  }
  
  #s_gbm <- exp(x_gbm)
  s_jd <- exp(x_jd)
  
  output <- list( "JD" = s_jd)
  
  return(output)
  
}

S0 <- 50
mu <- 0.05
r <- 0.05
t <- 1
sigma <- 0.3
N <- 252
M <- 100

# Frequency of jumps
lambda <- 4

# Jump process parameters a & b
a <- -0.05
b <- 0.1



# This jump diffusion model does not account for correlations between assets
# Maybe we should build a correlation matrix and pass that in for each equity too
# S0 should be update to be the first stock price for each equity right?

jump_diff_result <- Jump_diff(S0, mu, r, t, sigma, N, M, a, b, lambda)


# ------------------------------------------------------------------------------
# Simulate option prices using MC/GBM

call_price_MC <- function(S0, K, sigma, mu, t, N, M){
  
  set.seed(1730)
  
  Z <- rnorm(M)
  
  S_t <- S0 * exp((r - 0.5 * sigma^2) * t + sigma * sqrt(t) * Z)
  
  payoff <- pmax(S_t - K, 0)
  
  opt_price <- exp(-r * t) * mean(payoff)
  
  return(opt_price)
  
  
}


call_payoff <- function(ST, K){
  
  payoff <- pmax(ST - K, 0)
  return(payoff)
}


K <- 60

call_test <- call_price_MC(S0, K, sigma, mu, t, N, M)



# Calibrate to specific parameters
# Number of jumps, jump length, etc.
# Try initial model using the optim function/package in R


calibration_price_errors <- function(S0, options_data, rf){
  
  
  
  
}


