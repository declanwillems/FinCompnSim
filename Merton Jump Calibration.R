# Merton Jump-Diffusion Model
# Steen, Willems, Apostolico — Financial Comp & Sim Project

rm(list = ls())

library(ggplot2)
library(dplyr)
library(stats)

BLS <- function(S, K, T, r, sigma) {
  d1 <- (log(S/K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  C <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  return(C)
}

# finite sum of BS weighted by poisson prob

merton_price <- function(S0, K, T, r, sigma, a, b, lambda, n.max = 50) {
  n <- 0:n.max
  lambda_bar <- lambda * exp(a + 0.5 * b^2)
  coef <- (exp(-lambda_bar * T) * (lambda_bar * T)^n) / factorial(n)
  sigma_n <- sqrt(sigma^2 + (n * b^2 / T))
  r_n <- r - lambda * (exp(a + 0.5 * b^2) - 1) + (n * (a + 0.5 * b^2) / T)
  Vn <- sapply(1:length(n), function(i) BLS(S0, K, r_n[i], T, sigma_n[i]))
  return(sum(coef * Vn))
}

# calibration
calibrate_merton <- function(params, S0, K, T, r, market_price) {
  sigma <- params[1]; a <- params[2]; b <- params[3]; lambda <- params[4]
  model_price <- merton_price(S0, K, T, r, sigma, a, b, lambda)
  return((model_price - market_price)^2)
}

simulate_JD_paths <- function(S0, M, N, mu, r, sigma, a, b, t, lambda) {
  X.GBM <- X.JD <- matrix(NA, ncol = N + 1, nrow = M)
  X.GBM[,1] <- log(S0)
  X.JD[,1] <- log(S0)
  dt <- t / N
  sqdt <- sqrt(dt)
  for (i in 1:N) {
    Z <- rnorm(M)
    NN <- rpois(M, lambda * dt)
    Z2 <- rnorm(M)
    MM <- a * NN + b * sqrt(NN) * Z2
    X.JD[,i+1] <- X.JD[,i] + (mu - 0.5 * sigma^2) * dt + sigma * sqdt * Z + MM
    X.GBM[,i+1] <- X.GBM[,i] + (r - 0.5 * sigma^2) * dt + sigma * sqdt * Z
  }
  return(list(GBM = exp(X.GBM), JD = exp(X.JD)))
}

stock_data <- read.csv("act_stocks.csv")
option_data <- read.csv("act_options.csv")
stock_data$Date <- as.Date(stock_data$Date)
option_data$Trade.Date <- as.Date(option_data$Trade.Date)
option_data$Expiry.Date <- as.Date(option_data$Expiry.Date)
option_data$TTM <- as.numeric(difftime(option_data$Expiry.Date, option_data$Trade.Date, units = "days")) / 365


ticker <- "AAPL"
rf <- 0.045
stock_ticker <- stock_data %>% filter(Ticker == ticker)
option_ticker <- option_data %>% filter(Stock.Ticker == ticker)
merged <- left_join(option_ticker, stock_ticker, by = c("Trade.Date" = "Date", "Stock.Ticker" = "Ticker")) %>% na.omit()

results <- data.frame()

for (i in 1:nrow(merged)) {
  row <- merged[i, ]
  S0 <- row$Close; K <- row$Strike; TTM <- row$TTM; market_price <- row$Option.Mid.Price
  if (TTM <= 0 | market_price <= 0 | S0 <= 0) next
  init <- c(0.3, 0.01, 0.1, 5)
  lower <- c(0.01, -0.5, 0.01, 0.1)
  upper <- c(1, 0.5, 0.5, 20)
  opt <- tryCatch({
    optim(par = init, fn = calibrate_merton, method = "L-BFGS-B",
          lower = lower, upper = upper, S0 = S0, K = K, T = TTM, r = rf, market_price = market_price)
  }, error = function(e) NULL)
  if (is.null(opt) || opt$convergence != 0) next
  params <- opt$par
  model_price <- merton_price(S0, K, TTM, rf, params[1], params[2], params[3], params[4])
  results <- rbind(results, data.frame(
    Ticker = ticker, Strike = K, TTM = round(TTM, 4), Market.Price = market_price,
    Model.Price = round(model_price, 4), Sigma = round(params[1], 4),
    a = round(params[2], 4), b = round(params[3], 4), Lambda = round(params[4], 4),
    Error = round(abs(model_price - market_price), 4)
  ))
}


# 1 Calibration Error vs. TTM
plot1 <- ggplot(results, aes(x = TTM, y = Error)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Merton Calibration Error vs. Time to Maturity",
       x = "Time to Maturity (Years)", y = "Absolute Pricing Error") +
  theme_minimal()
plot(plot1)

# 2 Model Price vs Market Price
plot2 <- ggplot(results, aes(x = Market.Price, y = Model.Price, color = TTM)) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = paste("Merton vs Market Prices for", ticker),
       x = "Market Price", y = "Merton Model Price", color = "TTM") +
  theme_minimal(base_size = 14)
plot(plot2)

# 3 Monte Carlo Comparison
mu <- rf - mean(results$Lambda) * (exp(mean(results$a) + 0.5 * mean(results$b)^2) - 1)
sim <- simulate_JD_paths(150, 5000, 252, mu, rf, mean(results$Sigma), mean(results$a), mean(results$b), 0.5, mean(results$Lambda))
ST_JD <- sim$JD[, 253]
ST_GBM <- sim$GBM[, 253]

plot3 <- ggplot(data.frame(Price = c(ST_JD, ST_GBM), Model = rep(c("Jump-Diffusion", "GBM"), each = length(ST_JD))),
                aes(x = Price, fill = Model)) +
  geom_density(alpha = 0.5) +
  labs(title = "Simulated Terminal Prices: JD vs GBM", x = "Price", y = "Density") +
  theme_minimal()
plot(plot3)

# 4 Poisson Coefficient Visualization
n <- 0:100
lambda_bar <- mean(results$Lambda) * exp(mean(results$a) + 0.5 * mean(results$b)^2)
coef <- (exp(-lambda_bar * 0.5) * (lambda_bar * 0.5)^n) / factorial(n)

plot4 <- ggplot(data.frame(Jumps = n, Weight = coef), aes(x = Jumps, y = Weight)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Poisson Weights in Merton Expansion", x = "Number of Jumps", y = "Weight") +
  theme_minimal()
plot(plot4)