rm(list=ls())

# Libraries
library("ggplot2")
library("moments")
library(dplyr)
library(plotly)
library(tidyr)
library(akima)

# Load in cleaning file with project data
setwd("C:/Users/jacob/OneDrive/Documents/~RPI QFRA/2025 Spring Semester/Financial Computation and Simulation/Project")
source("Project Data Cleaning.R")

# Make the GBM Model iterative to get results for all tickers
tickers= c("AAPL", "AEP", "AEO", "ACN", "AFL", "ADM")

price_list= list(
  AAPL= aapl_price,
  AEP= aep_price,
  AEO= aeo_price,
  ACN= acn_price,
  AFL= afl_price,
  ADM= adm_price
)

results= vector("list",length(tickers))
names(results)= tickers

# Prepare container for results
results   <- list()

# Prepare container for results
results <- list()

results <- list()

for (ticker in tickers) {
  
  # 1) Load and sort prices
  asset_prices <- price_list[[ticker]] %>% arrange(Date)
  
  # 2) Daily log-returns
  R <- asset_prices %>%
    mutate(return = c(NA, diff(log(Close)))) %>%
    select(Date, return) %>%
    na.omit()
  
  # 3) Empirical moments
  my.stats <- tibble(
    mean     = mean(R$return),
    sd       = sd(R$return),
    skewness = skewness(R$return),
    kurtosis = kurtosis(R$return)
  )
  
  # 4) Annualize & simulate GBM
  dt    <- 1/252
  mu    <- my.stats$mean / dt
  sigma <- my.stats$sd * sqrt(1/dt)
  X0    <- 1
  M     <- nrow(R)
  
  stock_paths <- X0 * exp((mu - 0.5*sigma^2)*dt + sigma*sqrt(dt)*rnorm(M))
  R$stock_sim <- log(stock_paths / X0)
  
  # 5) Moments of simulated returns
  my.stats.sim <- tibble(
    mean     = mean(R$stock_sim / dt),
    sd       = sd(R$stock_sim * sqrt(1/dt)),
    skewness = skewness(R$stock_sim),
    kurtosis = kurtosis(R$stock_sim)
  )
  
  # 6) Build the option chain for our expiry
  target_date <- as.Date("2021-01-08")
  option_chain_real <- option_w_prices %>%
    filter(Stock.Ticker == ticker) %>%
    arrange(Trade.Date) %>%
    mutate(
      Expiration   = Expiry.Date,
      TTM          = as.numeric(difftime(Expiry.Date, Trade.Date, units = "days")),
      Strike       = as.numeric(Strike),
      Option.Price = as.numeric(Last.Trade.Price)
    ) %>%
    select(Trade.Date, Expiration, TTM, Strike, Option.Price)
  
  option_chain_of_interest <- option_chain_real %>%
    filter(Expiration == target_date)
  
  # 7) Closing price S0 (44 trading days before)
  S0 <- price_list[[ticker]] %>%
    filter(Date == (target_date - 44)) %>%
    pull(Close)
  
  # 8) GBM pricer
  euro_call_sim <- function(S0, K, r, sigma, t, nsteps, M) {
    Z  <- rnorm(M * nsteps)
    ST <- S0 * exp((r - 0.5*sigma^2)*t + sigma*sqrt(t)*Z)
    mean(exp(-r*t)*pmax(ST - K,0))
  }
  
  # 9) Price & MSE under GBM
  if (nrow(option_chain_of_interest) > 0) {
    set.seed(100)
    option_chain_of_interest$GBM_Price <- mapply(
      euro_call_sim,
      S0    = S0,
      K     = option_chain_of_interest$Strike,
      r     = 0.025,
      sigma = sigma,
      t     = option_chain_of_interest$TTM/252,
      MoreArgs = list(nsteps=252, M=10000),
      SIMPLIFY = TRUE
    )
    gbm_mse <- mean((option_chain_of_interest$GBM_Price - 
                       option_chain_of_interest$Option.Price)^2)
  } else {
    option_chain_of_interest$GBM_Price <- numeric(0)
    gbm_mse <- NA_real_
  }
  
  # 10) Store GBM results
  results[[ticker]] <- list(
    returns      = R,
    stats        = my.stats,
    sim_stats    = my.stats.sim,
    option_chain = option_chain_of_interest,
    gbm_mse      = gbm_mse
  )
  
  # 11) Jump-Diffusion pricer + calibrator
  euro_call_JD <- function(S0, K, r, sigma, lambda, mu_JD, sigma_JD, t, nsteps, M) {
    dt    <- t / nsteps
    kappa <- exp(mu_JD + 0.5*sigma_JD^2) - 1
    S     <- rep(S0, M)
    for (j in seq_len(nsteps)) {
      Z           <- rnorm(M)
      cont_factor <- exp((r - 0.5*sigma^2 - lambda*kappa)*dt + sigma*sqrt(dt)*Z)
      N_jumps     <- rpois(M, lambda*dt)
      Jump_factor <- rep(1, M)
      idx         <- which(N_jumps > 0)
      for (i in idx) {
        jumps <- rnorm(N_jumps[i], mean=mu_JD, sd=sigma_JD)
        Jump_factor[i] <- prod(exp(jumps))
      }
      S <- S * cont_factor * Jump_factor
    }
    mean(exp(-r*t)*pmax(S - K,0))
  }
  
  calibrate_JD <- function(params, S0, K_vec, t_vec, r, mkt_price) {
    sigma    <- params[1]; lambda   <- params[2]
    mu_JD    <- params[3]; sigma_JD <- params[4]
    sim_prices <- mapply(
      function(K, t) euro_call_JD(
        S0, K, r, sigma, lambda, mu_JD, sigma_JD, t,
        nsteps=252, M=3000
      ),
      K_vec, t_vec,
      SIMPLIFY = TRUE
    )
    mean((sim_prices - mkt_price)^2)
  }
  
  # 12) Pick one day for calibration
  cal_date     <- target_date
  one_day_chain <- option_chain_real %>%
    filter(Expiration==target_date, Trade.Date==cal_date) %>%
    transmute(
      Strike       = Strike,
      TTM          = TTM/252,
      Market.Price = Option.Price
    )
  
  # 13) JD calibration + pricing
  if (nrow(one_day_chain) > 2) {
    K_vec        <- one_day_chain$Strike
    t_vec        <- one_day_chain$TTM
    mkt_price    <- one_day_chain$Market.Price
    r            <- 0.025
    
    init_guess <- c(sigma=0.3, lambda=1, mu_JD=0, sigma_JD=0.2)
    lower      <- c(0.01, 0, -1, 0.01)
    upper      <- c(2,    10, 2,  1)
    
    set.seed(100)
    opt_JD <- optim(
      par          = init_guess,
      fn           = calibrate_JD,
      S0           = S0,
      K_vec        = K_vec,
      t_vec        = t_vec,
      r            = r,
      mkt_price    = mkt_price,
      method       = "L-BFGS-B",
      lower        = lower,
      upper        = upper,
      control      = list(trace=0, maxit=50)
    )
    best_param <- opt_JD$par
    
    jd_vals <- mapply(
      function(K, t) euro_call_JD(
        S0, K, r,
        best_param["sigma"],
        best_param["lambda"],
        best_param["mu_JD"],
        best_param["sigma_JD"],
        t, nsteps=252, M=10000
      ),
      K_vec, t_vec,
      SIMPLIFY = TRUE
    )
    
    compare_JD <- tibble(
      Strike   = K_vec,
      TTM       = t_vec,
      Market    = mkt_price,
      JD_Model  = jd_vals
    ) %>% mutate(JD_Error = (JD_Model - Market)^2)
    
    MSE_JD <- mean(compare_JD$JD_Error)
  } else {
    best_param <- rep(NA_real_, 4)
    compare_JD <- tibble(Strike=numeric(0), TTM=numeric(0),
                         Market=numeric(0), JD_Model=numeric(0), JD_Error=numeric(0))
    MSE_JD     <- NA_real_
  }
  
  # 14) Store JD results
  results[[ticker]]$JD_params  <- best_param
  results[[ticker]]$JD_compare <- compare_JD
  results[[ticker]]$MSE_JD     <- MSE_JD
  
  # 15) Build GBM surface
  df_gbm <- option_chain_of_interest %>%
    mutate(GBM_Error = (GBM_Price - Option.Price)^2)
  
  if (nrow(df_gbm) >= 3) {
    ig <- akima::interp(x=df_gbm$TTM, y=df_gbm$Strike, z=df_gbm$GBM_Error, duplicate="mean")
    results[[ticker]]$GBM_surface <- plot_ly(x=ig$x, y=ig$y, z=ig$z, type="surface") %>%
      layout(title=paste0(ticker," GBM MSE"), scene=list(
        xaxis=list(title="TTM"), yaxis=list(title="Strike"), zaxis=list(title="MSE")
      ))
  } else {
    results[[ticker]]$GBM_surface <- NULL
  }
  
  # 16) Build JD surface (only that one day)
  if (nrow(compare_JD) >= 3) {
    ij <- akima::interp(x=compare_JD$TTM, y=compare_JD$Strike, z=compare_JD$JD_Error, duplicate="mean")
    results[[ticker]]$JD_surface <- plot_ly(x=ij$x, y=ij$y, z=ij$z, type="surface") %>%
      layout(title=paste0(ticker," JD MSE"), scene=list(
        xaxis=list(title="TTM"), yaxis=list(title="Strike"), zaxis=list(title="MSE")
      ))
  } else {
    results[[ticker]]$JD_surface <- NULL
  }
  
}  # end for each ticker
