# Financial Computation and Simulation
# Project 2025
# Declan Willems & Jacob Steen

# File for cleaning data

rm(list = ls())


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


# Build GBM Model

