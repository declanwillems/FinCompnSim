# Refactored Jump Diff w/ plots
rm(list = ls())
# Libraries
library("ggplot2")
library("moments")
library(dplyr)
library(plot3D)
library(tidyr)


aapl_price     <- stock_data %>% 
  filter(Ticker=="AFL") %>%
  arrange(Date) %>%
  mutate(Date = as.Date(Date))

# actual daily log-returns
R <- aapl_price %>%
  mutate(ret = log(Close/lag(Close))) %>%
  select(Date, ret) %>%
  na.omit()

# target option chain date
option_w_prices <- read.csv("act_options_prices.csv") %>%
  filter(Stock.Ticker=="AFL") %>%
  mutate(
    Expiry.Date = as.Date(Expiry.Date),
    Trade.Date  = as.Date(Trade.Date),
    MidPrice    = (Bid.Price+Ask.Price)/2,
    TTM         = as.numeric(difftime(Expiry.Date, Trade.Date,"days"))/365
  ) %>%
  filter(MidPrice>0, TTM>0) %>%
  arrange(Expiry.Date)

target_date <- as.Date("2021-01-08")
chain_30d   <- option_w_prices %>% filter(Expiry.Date==target_date)

# closing price 30 days prior:
S0_chain <- aapl_price %>% filter(Date==target_date - 30) %>% pull(Close)

# —————————————————————————
# 2) Moments of actual returns
# —————————————————————————
actual_stats <- c(
  mean    = mean(R$ret),
  sd      = sd(R$ret),
  skew    = skewness(R$ret),
  kurt    = kurtosis(R$ret)
)
print(actual_stats)


yearly_stats <- actual_stats * 252
print(yearly_stats)
# —————————————————————————
# 3) Set up parameters for simulation
# —————————————————————————
# from calibration or your default
r      <- 0.045
dt     <- 1/252
mu     <- actual_stats["mean"]/dt
sigma  <- actual_stats["sd"]*sqrt(1/dt)

# Merton params (pick from your earlier 'results' averages or defaults)
a      <- 0.01
b      <- 0.1
lambda <- 5

# helper to simulate one‐day JD and GBM returns
one_day_sims <- function(M) {
  # GBM step
  Z   <- rnorm(M)
  S_g <- exp((mu-0.5*sigma^2)*dt + sigma*sqrt(dt)*Z)
  
  # JD step
  N_j <- rpois(M, lambda*dt)
  Z2  <- rnorm(M)
  jump_term <- a*N_j + b*sqrt(N_j)*Z2
  S_j <- exp((mu-0.5*sigma^2)*dt + sigma*sqrt(dt)*Z + jump_term)
  
  data.frame(
    ret_real = R$ret,                # will recycle but we only need length(M)
    ret_gbm  = log(S_g),
    ret_jd   = log(S_j)
  )
}

# simulate M = number of actual observations
M <- nrow(R)
sim_df <- one_day_sims(M)

# —————————————————————————
# 4) Moments of simulated returns
# —————————————————————————
sim_stats <- data.frame(
  Model = c("GBM","JD"),
  Mean  = c(mean(sim_df$ret_gbm),  mean(sim_df$ret_jd)) * 252,
  SD    = c(sd(sim_df$ret_gbm),    sd(sim_df$ret_jd)) * 252,
  Skew  = c(skewness(sim_df$ret_gbm), skewness(sim_df$ret_jd)),
  Kurt  = c(kurtosis(sim_df$ret_gbm), kurtosis(sim_df$ret_jd))
)
print(sim_stats)


# —————————————————————————
# 5) Overlay histograms: real vs GBM vs JD
# —————————————————————————
plot_df <- bind_rows(
  data.frame(ret= R$ret,      Model="Real"),
  data.frame(ret= sim_df$ret_gbm, Model="GBM"),
  data.frame(ret= sim_df$ret_jd,  Model="JD")
)

ggplot(plot_df, aes(x=ret, fill=Model)) +
  geom_histogram(aes(y=..density..), bins=100, alpha=0.4, position="identity") +
  stat_function(
    fun = dnorm,
    args = list(mean=actual_stats["mean"], sd=actual_stats["sd"]),
    color="black", size=1
  ) +
  ggtitle("AFL Daily LogReturn: Real vs GBM vs Jump‐Diffusion") +
  theme_minimal()

# —————————————————————————
# 6) Option chain pricing: BSM vs GBM‐MC vs JD‐MC
# —————————————————————————
# Black‐Scholes:
BSM_call <- function(S0,K,r,σ,T){
  d1 <- (log(S0/K) + (r+0.5*σ^2)*T)/(σ*sqrt(T))
  d2 <- d1 - σ*sqrt(T)
  S0*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
}

# Monte‑Carlo GBM pricer
mc_gbm <- function(S0,K,r,σ,T,M=10000){
  Z <- rnorm(M)
  ST <- S0 * exp((r-0.5*σ^2)*T + σ*sqrt(T)*Z)
  mean(pmax(ST-K,0))*exp(-r*T)
}

# Monte‑Carlo JD pricer
mc_jd <- function(S0,K,r,σ,a,b,λ,T,M=10000){
  dt <- T/round(252*T)         # daily steps
  Nsteps <- round(252*T)
  paths <- simulate_JD_paths(S0, M, Nsteps, mu, r, σ, a, b, T, λ)$JD
  ST <- paths[, Nsteps+1]
  mean(pmax(ST-K,0))*exp(-r*T)
}

# build chain
chain <- chain_30d %>%
  select(Strike, TTM, MidPrice) %>%
  distinct()

chain$BSM   <- mapply(BSM_call,   S0=S0_chain, K=chain$Strike, r=r, σ=sigma, T=chain$TTM)
chain$GBMMC <- mapply(mc_gbm,     S0=S0_chain, K=chain$Strike, r=r, σ=sigma, T=chain$TTM)
chain$JDMC  <- mapply(mc_jd,      S0=S0_chain, K=chain$Strike, r=r, σ=sigma,
                      a=a, b=b, λ=lambda, T=chain$TTM)

# reshape for plot
long_chain <- chain %>%
  pivot_longer(c("BSM","GBMMC","JDMC"), names_to="Model", values_to="Price")

# plot
ggplot(long_chain, aes(x=Strike, y=Price, color=Model)) +
  geom_line(size=1) +
  geom_point(size=2, alpha=0.7) +
  ggtitle("AAPL Option Prices: BSM vs GBM‑MC vs JD‑MC (30‑day)") +
  theme_minimal()

# MSEs
mse_gbm <- mean((chain$MidPrice - chain$GBMMC)^2)
mse_jd  <- mean((chain$MidPrice - chain$JDMC )^2)
cat("MSE GBM‑MC:", round(mse_gbm,4), "\n")
cat("MSE JD‑MC: ", round(mse_jd,4), "\n")