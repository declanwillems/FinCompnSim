# Financial Computation and Simulation
# Project 2025
# Declan Willems & Jacob Steen

# File for cleaning data

rm(list = ls())

library("plotly")
# Package to connect points in scatter plot to make a surface
# Utilized in part 1 for IV plot
library("akima")


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


# Implied Volatility Surfaces from observed option prices for all stocks


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


Jump_diff <- function(S0, mu, r, t, sigma, N, M){
  
  
  
}
