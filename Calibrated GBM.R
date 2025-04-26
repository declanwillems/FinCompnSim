# Steen, Willems and Apostolico Financial Comp and Sim Project
# Calibrate the model for a single stock
rm(list=ls())

# Libraries
library("ggplot2")
library("moments")
library(dplyr)
library(plot3D)
library(tidyr)

# Load in cleaning file with project data
setwd("C:/Users/jacob/OneDrive/Documents/~RPI QFRA/2025 Spring Semester/Financial Computation and Simulation/Project")
source("Project Data Cleaning.R")

# Returns on single stock
asset_prices= aapl_price %>% arrange(Date)
R= asset_prices %>%
  mutate(
    return= c(NA,diff(log(Close)))
  ) %>%
  select(Date,return)
R= na.omit(R)
# Summarize the moments of the assets:
my.stats= as.data.frame(matrix(NA,nrow=4,ncol=1))
#names(my.stats)= names(R)
my.stats[1,]= mean(R$return)
my.stats[2,]= sd(R$return)
my.stats[3,]= skewness(R$return)
my.stats[4,]= kurtosis(R$return)
print(my.stats,digits=5)

# Plot price distribution
tics= asset_prices
pts= list(NA)
n= 2

ggplot(R, aes(x = return)) + 
  geom_histogram(aes(y = ..density..),
                 bins = 100,
                 colour = "black", 
                 fill = "white") +
  stat_function(fun = dnorm,
                args = list(mean = my.stats[1, 1],
                            sd = sqrt(my.stats[2, 1]),
                            kurtosis=my.stats[4,1],
                            skewness= my.stats[3,1])) +
  ggtitle("Single Stock")

# Daily to annualized returns
dt= 1/252
mu= mean(R$return)/dt
sigma= sd(R$return)*sqrt(1/dt) 

# init price (According to class notes, this doesn't matter so set to 1)
X0= 1.0

# Number of simulations
n.sim= nrow(R) # This makes things easier later

# Simulate price paths (This is the GBM)
t= 1/252
stock_paths= X0*exp((mu - 0.5*sigma^2)*t+sigma*sqrt(t)*rnorm(n.sim))
stock_paths_R= log(stock_paths/X0) # log returns of stock paths
R$stock_sim= stock_paths_R
d= density(R$stock_sim)

# Summarize the moments of the assets:
my.stats.sim= as.data.frame(matrix(NA,nrow=4,ncol=1))
#names(my.stats)= names(R)
my.stats.sim[1,]= mean(stock_paths_R/dt)
my.stats.sim[2,]= sd(stock_paths_R*sqrt(1/dt))
my.stats.sim[3,]= skewness(stock_paths_R)
my.stats.sim[4,]= kurtosis(stock_paths_R)
print(my.stats.sim,digits=2)


# Stack results
M= nrow(R)
data= data.frame(
  x= rbind(matrix(R$return,ncol=1),
            matrix(R$stock_sim,ncol=1)),
  y = rep(c("Stock","Sim"),each = M))
data

my.plot= ggplot(data,aes(x=x)) + 
  geom_histogram(data=subset(data,y == 'Stock'),aes(y =..density..),
                 bins=100,
                 colour = "black", 
                 fill = "blue",
                 alpha=0.5) +
  geom_histogram(data=subset(data,y == 'Sim'),aes(y =..density..),
                 bins=100,
                 colour = "black", 
                 fill = "yellow",
                 alpha = 0.5) + 
  stat_function(fun = dnorm, args = list(mean = mu*dt, sd = sigma*sqrt(dt)))+
  ggtitle("AEP Stock versus Simulated Data")

print(my.plot)

# Price the call options and make an option chain
# Get AAPL option data
option_chain_real= option_w_prices %>%
  filter(Stock.Ticker=="AAPL") %>%
  arrange(Trade.Date) %>%
  mutate(
    Expiration= Expiry.Date,
    Trade.Date= Trade.Date,
    TTM= as.numeric(difftime(Expiry.Date, Trade.Date,units='days')),
    Strike= Strike,
    Option.Price= Last.Trade.Price,
  ) %>%
  select(Trade.Date, Expiration,TTM,Strike,Option.Price)

target_date= as.Date("2021-01-08")

option_chain_of_interest= option_chain_real %>%
  filter(Expiration==target_date) %>%
  mutate(
    Expiration= "2021-01-08"
  )


# Filter for aapl closing price to get implied vol on these contracts
closing_price= aapl_price %>%
  filter(Date==(target_date)-44) %>% # This is the latest TTM
  pull(Close)
closing_price

# Price euro calls and plot price surface and compare BSM versus simulated

BSM_call= function(S0,K,r,sigma,T){
  d1= (log(S0/K)+(r+0.5*sigma^2)*t)/(sigma*sqrt(t))
  d2= d1-sigma*sqrt(t)
  call_price= S0*pnorm(d1)-K*exp(-r*t)*pnorm(d2)
  return(call_price)
}

euro_call_sim=function(S0,K,r,sigma,t,nsteps,M){
  
  dt= t/nsteps
  Z= matrix(rnorm(M*nsteps),nrow=M,ncol=nsteps)
  ST= S0*exp((r-0.5*sigma^2)*t+sigma*sqrt(t)*Z)
  payoff= pmax(ST-K,0)
  discounted_payoff= exp(-r*t)*payoff
  est_price= mean(discounted_payoff)
  # sd_est_price= sd(discounted_payoff)
  # z= qnorm(1-0.05/2) # This is running on both tails
  # CI= c(est_price-z*sd_est_price/sqrt(N),est_price+2*sd_est_price/sqrt(N))
  return(est_price)
}

set.seed(100)

# Loop through strikes and store results for BSM

# Now it loops through all the rows, not just the first
# Changed seq_along to seq_len
for (i in seq_len(nrow(option_chain_of_interest))){
  option_chain_of_interest$Euro.Price[i]= euro_call_sim(
    S0= 113.8711,
    K= option_chain_of_interest$Strike[i],
    r= 0.025,
    sigma= 0.3249201,
    t= (option_chain_of_interest$TTM)/252,
    nsteps=252,
    M= 10000
  )
}


# ------------------------------------------------------------------------------
# Jump-Diffusion model 


euro_call_JD <- function(S0, K, r, sigma, lambda, mu_JD, sigma_JD, t, nsteps, M){
  
  dt <- t / nsteps
  
  kappa <- exp(mu_JD + 0.5 * sigma_JD^2) - 1
  
  # Init matrix with prices
  S <- rep(S0, M)
  
  for (j in seq_len(nsteps)){
    
    Z <- rnorm(M)
    cont_factor <- exp((r - 0.5 * sigma^2 - lambda * kappa)* dt + sigma * sqrt(dt) * Z)
    
    N_jumps <- rpois(M, lambda * dt)
    
    Jump_factor <- rep(1, M)
    
    # See which paths has a jump
    jump_count <- which(N_jumps > 1)
    
    for (i in jump_count){
      
      jump <- rnorm(N_jumps[i], mean = mu_JD, sd = sigma_JD)
      
      Jump_factor[i] <- prod(exp(jump))
    }
    
    # Update spot price
    
    S <- S * cont_factor * Jump_factor
    
  }
  
  # Payoff and price
  
  payoff <- pmax(S - K, 0)
  
  opt_price <- mean(exp(-r * t) * payoff)
  
  return(opt_price)
  
}


# Calibration function

calibrate_JD <- function(params, S0, K_vec, t_vec, r, market_price){
  
  sigma <- params[1]
  lambda <- params[2]
  mu_JD <- params[3]
  sigma_JD <- params[4]
  
  
  sim_prices_JD <- mapply(
    FUN = function(K, t){
      euro_call_JD(S0, K, r, sigma, lambda, mu_JD, sigma_JD, t,
                   nsteps = 252, M = 3000) # Attempt lower paths for ease of optimization
    },
    K = K_vec, t = t_vec
  )
  
  
  # MSE
  
  mse_JD <- mean((sim_prices_JD - market_price)^2)
  
  return(mse_JD)
  
}

# Specific day to analyze, defined above in GBM section 
# option_chain_of_interest


# Reduce down to 1 trade date and 1 expr

cal_date <- as.Date("2020-11-25")

one_day_chain <- option_chain_real %>%
  filter(
    Expiration == target_date,
    Trade.Date == cal_date
  ) %>%
  select(Strike, TTM, Market.Price = Option.Price)

# Assuming this is close price of the day we are analyzing
S0 <- 113.8711
K_vec <- one_day_chain$Strike
t_vec <- one_day_chain$TTM / 365
market_price <- one_day_chain$Market.Price
r <- 0.025

# Initialize parameter guesses

init_guess <- c(sigma = 0.3, lambda = 1, mu_JD = 0, sigma_JD = 0.2)

lower_bound <- c(0.01, 0, -1, 0.01)
upper_bound <- c(2, 10, 2, 1)


set.seed(100)


optimized_JD <- optim(
  par = init_guess,
  fn = calibrate_JD,
  S0 = S0,
  K_vec = K_vec,
  t_vec = t_vec,
  r = r,
  market_price = market_price,
  method = "L-BFGS-B", # quasi-Netweon solver that takes in upper and lower constr.
  lower = lower_bound,
  upper = upper_bound,
  control = list(trace = 1, maxit = 50)
)

best_param <- optimized_JD$par
