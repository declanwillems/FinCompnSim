# Merton Jump-Diffusion Model
# Steen, Willems, Apostolico — Financial Comp & Sim Project

rm(list = ls())

library("ggplot2")
library("dplyr")
library("stats")

# Load data
setwd("C:\\Users\\willed3\\OneDrive - Rensselaer Polytechnic Institute\\Documents\\Spring 2025\\Financial Computation & Simulation\\Project\\Data")

stock_data <- read.csv("act_stocks.csv")
option_w_prices <- read.csv("act_options_prices.csv")

ticker_choices <- c("AAPL", "AEP", "AEO", "ACN", "AFL", "ADM")

# Filter and join 
stock_data <- stock_data %>% filter(Ticker %in% ticker_choices)
option_data <- option_w_prices %>% filter(Stock.Ticker %in% ticker_choices)

# Mid price
option_data <- option_data %>%
  mutate(Mid.Price = (Bid.Price + Ask.Price) / 2) %>%
  filter(Mid.Price > 0, Strike > 0)

stock_data$Date <- as.Date(stock_data$Date)
option_data$Trade.Date <- as.Date(option_data$Trade.Date)
option_data$Expiry.Date <- as.Date(option_data$Expiry.Date)
option_data$TTM <- as.numeric(difftime(option_data$Expiry.Date, option_data$Trade.Date, units = "days")) / 365

merged <- option_data %>%
  left_join(stock_data %>% select(Date, Ticker, Close), by = c("Trade.Date" = "Date", "Stock.Ticker" = "Ticker")) %>%
  filter(!is.na(Close) & !is.na(Mid.Price) & TTM > 0 & Mid.Price > 0 & Close > 0)

# BLS 
BLS <- function(S, K, T, r, sigma) {
  d1 <- (log(S/K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
}

# Merton pricing function
merton_price <- function(S0, K, T, r, sigma, a, b, lambda, n.max = 50) {
  n <- 0:n.max
  lambda_bar <- lambda * exp(a + 0.5 * b^2)
  coef <- (exp(-lambda_bar * T) * (lambda_bar * T)^n) / factorial(n)
  sigma_n <- sqrt(sigma^2 + (n * b^2 / T))
  r_n <- r - lambda * (exp(a + 0.5 * b^2) - 1) + (n * (a + 0.5 * b^2) / T)
  Vn <- sapply(1:length(n), function(i) BLS(S0, K, T, r_n[i], sigma_n[i]))
  model_val <- sum(coef * Vn)
  if (is.na(model_val) || model_val < 1e-8) model_val <- 0  # catch tiny values
  if (model_val > 0) {
    print(paste("Model Value:", model_val))
  } 

  return(model_val)
}

# Calibration loss function
calibrate_merton <- function(params, S0, K, T, r, market_price) {
  sigma <- params[1]; a <- params[2]; b <- params[3]; lambda <- params[4]
  model_price <- merton_price(S0, K, T, r, sigma, a, b, lambda)
  return((model_price - market_price)^2)
}

# Run calibration
results <- data.frame()
merged_sample <- merged[1:10000, ] # only 10000 sample rows run 

for (i in 1:nrow(merged_sample)) {
  row <- merged_sample[i, ]
  S0 <- row$Close
  K <- row$Strike
  TTM <- row$TTM
  market_price <- row$Mid.Price
  
  if (any(is.na(c(S0, K, TTM, market_price))) || S0 <= 0 || K <= 0 || TTM <= 0 || market_price <= 0) next
  
  model_price <- merton_price(S0, K, TTM, 0.045, 0.3, 0.01, 0.1, 5)
  
  results <- rbind(results, data.frame(
    Ticker = row$Stock.Ticker, Strike = K, TTM = round(TTM, 4), Market.Price = market_price,
    Model.Price = round(model_price, 4), Sigma = 0.3,
    a = 0.01, b = 0.1, Lambda = 5,
    Error = round(abs(model_price - market_price), 4)
  ))
}

# Visual 1: Poisson Coefficient Drop
n <- seq(0, 100)
lambda_est <- mean(results$Lambda, na.rm = TRUE)
a_est <- mean(results$a, na.rm = TRUE)
b_est <- mean(results$b, na.rm = TRUE)
t <- 1
lambda_bar <- lambda_est * exp(a_est + 0.5 * b_est^2)
coef <- (exp(-lambda_bar * t)*(lambda_bar * t)^n) / (factorial(n))

plot1 <- ggplot(data.frame(n = n, coef = coef), aes(x = n, y = coef)) +
  geom_line(color = "steelblue") +
  ggtitle("Poisson Coefficients in Merton Expansion") +
  xlab("Number of Jumps") + ylab("Weight") +
  theme_minimal()
print(plot1)

# Visual 2: JD vs GBM Monte Carlo Simulation
simulate_JD_paths <- function(S0,M,N,mu,r,sigma,a,b,t,lambda){
  X.GBM <- X.JD <-matrix(NA,ncol=N+1,nrow=M)
  X.GBM[,1] <- log(S0)
  X.JD[,1] <- log(S0)
  dt <- t/N
  sqdt <- sqrt(dt)
  for (i in 1:N){
    Z <- matrix(rnorm(M),ncol=1)
    NN <- matrix(rpois(M,lambda*dt),ncol=1)
    Z2 <- matrix(rnorm(M),ncol=1)
    MM <- a*NN + b*sqrt(NN)*Z2
    X.JD[,i+1] <- X.JD[,i] + (mu - 0.5*sigma^2)*dt + sigma*sqdt*Z + MM
    X.GBM[,i+1] <- X.GBM[,i] + (r - 0.5*sigma^2)*dt + sigma*sqdt*Z
  }
  S.GBM <- exp(X.GBM)
  S.JD <- exp(X.JD)
  out <- list("GBM"=S.GBM,"JD"=S.JD)
  return(out)
}

if (!is.na(lambda_est) && !is.na(a_est) && !is.na(b_est)) {
  mu <- 0.045 - lambda_est * (exp(a_est + 0.5*b_est^2) - 1)
  JD.sim <- simulate_JD_paths(150, 10000, 252, mu, 0.045,
                              sigma = mean(results$Sigma, na.rm = TRUE),
                              a = a_est, b = b_est, t = 1, lambda = lambda_est)
  ST.J <- JD.sim$JD[,253]
  ST.G <- JD.sim$GBM[,253]

  payoff <- function(ST,K) pmax(ST-K,0)
  K <- 150
  CJ <- exp(-0.045*1)*mean(payoff(ST.J,K))
  CG <- exp(-0.045*1)*mean(payoff(ST.G,K))

  cat("Jump-Diffusion Monte Carlo Price:", CJ, "\n")
  cat("GBM Monte Carlo Price:", CG, "\n")

  df <- data.frame(Price = c(ST.J, ST.G),
                   Model = rep(c("Jump-Diffusion", "GBM"), each = length(ST.J)))
  plot2 <- ggplot(df, aes(x = Price, fill = Model)) +
    geom_density(alpha = 0.5) +
    labs(title = "Terminal Price Distribution: JD vs GBM", x = "Price", y = "Density") +
    theme_minimal()
  print(plot2)
}
