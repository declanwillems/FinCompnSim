# Comp & Sim 
# Final Project 

# Data initialization, cleaning, and IV surface plots

rm(list = ls())

library("plotly")
# Package to connect points in scatter plot to make a surface
# Utilized in part 1 for IV plot
library("akima")
library("dplyr")

#setwd("C:/Users/jacob/OneDrive/Documents/~RPI QFRA/2025 Spring Semester/Financial Computation and Simulation/Project")
setwd("C:/Users/willed3/OneDrive - Rensselaer Polytechnic Institute/Documents/Spring 2025/Financial Computation & Simulation/Project/Data")

stock_data <- read.csv("act_stocks.csv")

# Stock data extracted
# AAPL
aapl_price= stock_data %>%
  filter(Ticker=="AAPL") %>%
  arrange(Date) %>%
  mutate(
    return= Close
  )
# AEP
aep_price= stock_data %>%
  filter(Ticker=="AEP") %>%
  arrange(Date) %>%
  mutate(
    return= Close
  )
# AEO
aeo_price= stock_data %>%
  filter(Ticker=="AEO") %>%
  arrange(Date) %>%
  mutate(
    return= Close
  )
# ACN
acn_price= stock_data %>%
  filter(Ticker=="ACN") %>%
  arrange(Date) %>%
  mutate(
    return= Close
  )
# AFL
afl_price= stock_data %>%
  filter(Ticker=="AFL") %>%
  arrange(Date) %>%
  mutate(
    return= Close
  )
# ADM
adm_price= stock_data %>%
  filter(Ticker=="ADM") %>%
  arrange(Date) %>%
  mutate(
    return= Close
  )

head(aapl_price)
head(aep_price)
head(aeo_price)
head(acn_price)
head(afl_price)
head(adm_price)

option_wo_price <- read.csv("act_options.csv")

option_w_prices <- read.csv("act_options_prices.csv")

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


stock_filter <- stock_data[stock_data$Ticker %in% ticker_choices, ]

options_woPrice_filtered <- option_wo_price[option_wo_price$Stock.Ticker %in% ticker_choices, ]
options_wPrice_filtered <- option_w_prices[option_w_prices$Stock.Ticker %in% ticker_choices, ]



# stock_filtered <- stock_data[stock_data$Ticker %in% ticker_choices, ]
# 
# #options_filtered <- option_data[option_data$Stock.Ticker %in% ticker_choices, ]
# 
# options_filtered <- option_data_with_prices[option_data_with_prices$Stock.Ticker %in% ticker_choices, ]
# 
# options_without_prices_filtered <- option_data[option_data$Stock.Ticker %in% ticker_choices, ]
# 
# options_filtered$Avg.IV <- options_without_prices_filtered$Avg.IV

# merge_n_filter <- function(stock_data, option_w_price, option_wo_price, ticker_choices){
#   
#   
#   stock_filter <- stock_data[stock_data$Ticker %in% ticker_choices, ]
#   
#   options_woPrice_filter <- option_wo_price[option_wo_price$Ticker %in% ticker_choices, ]
#   
#   options_wPrice_filter <- option_w_price[option_w_price %in% ticker_choices, ]
#   
#   options_wPrice_filter$Avg.IV <- option_woPrice_filter$Avg.IV
#   
#   
#   stock_filtered <- na.omit(stock_filtered)
#   options_filtered <- na.omit(options_filtered)
#   
#   return(stock_filter = stock_filter, options_filter = options_filter)
#   
# }
# 
# 
# filted_results <- merge_n_filter(stock_data, option_data_with_prices, option_data, ticker_choices)
# 


# Daily and annual mean and std dev of each desired ticker
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
  
  ticker_calibration(ticker, stock_filter)
}))

head(calibrated_values)

# ENsuring data is in Date format
stock_filter$Date <- as.Date(stock_filter$Date)

options_woPrice_filtered$Trade.Date <- as.Date(options_woPrice_filtered$Trade.Date)

options_woPrice_filtered$Expiry.Date <- as.Date(options_woPrice_filtered$Expiry.Date)

options_woPrice_filtered$TTM <- as.numeric(difftime(options_woPrice_filtered$Expiry.Date, options_woPrice_filtered$Trade.Date, units = "days")) / 365

options_wPrice_filtered$Trade.Date <- as.Date(options_wPrice_filtered$Trade.Date)

options_wPrice_filtered$Expiry.Date <- as.Date(options_wPrice_filtered$Expiry.Date)

options_wPrice_filtered$TTM <- as.numeric(difftime(options_wPrice_filtered$Expiry.Date, options_wPrice_filtered$Trade.Date, units = "days")) / 365


ticker_check <- unique(options_wPrice_filtered$Stock.Ticker)

ticker_check_stocks <- unique(stock_filter$Ticker)

# merged_stock_n_options <- left_join(options_wPrice_filtered, stock_filter,
#                                     by = c("Trade.Date" = "Date", "Stock.Ticker" = "Ticker"))

merged_stock_n_options <- options_wPrice_filtered %>%
  left_join(
    stock_filter %>% select(Date, Ticker, Close, Volume),
    by = c("Trade.Date" = "Date", "Stock.Ticker" = "Ticker")
  )

# Dropping useless column

merged_stock_n_options <- merged_stock_n_options %>%
  select(-starts_with("X."))

# Ensuring stock ticker matches up with its true date and close price

merged_stock_n_options %>%
  arrange(Stock.Ticker, Trade.Date) %>%
  head(10)

merged_tickers <- unique(merged_stock_n_options$Stock.Ticker)


print(ticker_check_stocks)

print(merged_tickers)

# Log returns for summary stat calcs
stock_filter <- stock_filter %>%
  arrange(Ticker, Date) %>%
  group_by(Ticker) %>%
  mutate(LogReturn = log(Close / lag(Close))) %>%
  ungroup()

# Summary stats for each of the firms
stock_summary <- stock_filter %>%
  dplyr::group_by(Ticker) %>%
  dplyr::summarise(
    StartDate   = min(Date, na.rm=TRUE),
    EndDate     = max(Date, na.rm=TRUE),
    MeanClose   = mean(Close, na.rm=TRUE),
    MedianClose = median(Close, na.rm=TRUE),
    MinClose    = min(Close, na.rm=TRUE),
    MaxClose    = max(Close, na.rm=TRUE),
    AvgDailyRet  = mean(LogReturn, na.rm = TRUE),
    AnnualRet    = AvgDailyRet * 252,
    DailyVol     = sd(LogReturn, na.rm = TRUE),
    AnnualVol    = DailyVol * sqrt(252),
    TotalReturn = last(Close) / first(Close) - 1,
    AvgVolume   = mean(Volume, na.rm=TRUE),
    .groups = "drop"
  )

print(stock_summary)


# Summarize options
options_summary <- options_woPrice_filtered %>%
  # ensure it’s the dplyr grouping
  dplyr::group_by(Stock.Ticker) %>%
  dplyr::summarise(
    NQuotes         = dplyr::n(),              # count of rows per group
    AvgIV           = mean(Avg.IV, na.rm = TRUE),
    MinIV           = min(Avg.IV, na.rm = TRUE),
    MaxIV           = max(Avg.IV, na.rm = TRUE),
    AvgTTM          = mean(TTM, na.rm = TRUE),
    AvgOpenInterest = mean(Open.Interest, na.rm = TRUE),
    AvgOptVolume    = mean(Volume, na.rm = TRUE),
    .groups = "drop"                           # drop grouping afterwards
  )

print(options_summary)


# Implied Volatility Surfaces from observed option prices for all stocks

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


plot_IVs <- plot_IV_surface(options_woPrice_filtered, ticker_choices)



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

call_price_test <- option_price_backout(stock_filtered, options_woPrice_filtered, rf)


# Build pricing model for American options use an implicit or explicit method
# Above function only works for Euro options as per Black-Scholes

option_pricing= function(S0,K,r,t,sigma,N=200,call=1,euro=0,crr=1){
  dt= t/N
  # Determine payoff function:
  if (call == 1){
    h= function(S){
      h= pmax(S - K,0) # Call payoff
    }
  }
  else {
    h= function(S){
      h= pmax(K - S,0) # Put payoff
    }
  }
  
  # Euro vs. Amer:
  if (euro == 1){
    A= 0
  }
  else{
    A= 1
  }
  # Determine parameterization method:
  if (crr == 1){
    u= exp(sigma*sqrt(dt))
    d= 1/u
    p= (exp(r*dt) - d) / (u - d)
  }
  else{ # JRR Model
    u= exp((r-sigma*sigma/2)*dt + sigma*sqrt(dt))
    d= exp((r-sigma*sigma/2)*dt - sigma*sqrt(dt))
    p= 1/2
  }
  
  # initialize the vector:
  vec= rep(NA,length=(2*N+1))
  
  # Populate the terminal payoffs:
  NN= length(vec)
  nu= matrix(seq(N,0),ncol=1)
  nd= matrix(seq(0,N),ncol=1)
  vec[seq(1,NN,by=2)]= h(S0 * u ^ nu * d ^ nd)
  disc= exp(-r*dt)
  q= 1 - p
  dp= disc*p
  dq= disc*q
  
  # Solve by backward induction:
  for (i in 1:N){
    opts= vec[seq(i,NN-i+1,by=2)]
    nu= seq(N-i,0)
    nd= seq(0,N-i)
    # Intrinsic value of the option (Multiply by A = 1 if American; A=0 for European)
    intrin= h(S0 * u^nu * d^nd) * A
    
    vec[seq(1+i,NN-i,by=2)]= pmax((dp*opts[1:(length(opts)-1)] +
                                       dq*opts[2:(length(opts))]),
                                    intrin)
  }
  x= vec[N+1]
  return(x)
}
