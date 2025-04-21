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
    TTM= difftime(Expiry.Date, Trade.Date,units='days'),
    Strike= Strike,
    Option.Price= Last.Trade.Price,
  ) %>%
  select(Expiration,TTM,Strike,Option.Price)

target_date= as.Date("2021-01-08")

thirty_day_option_chain= option_chain_real %>%
  filter(Expiration==target_date) %>%
  mutate(
    TTM= "30 days"
  )
avg_strike_chain= thirty_day_option_chain %>%
  group_by(Strike) %>%
  summarise(
    avg_price= mean(Option.Price, na.rm=TRUE)
  )
aapl_price= aapl_price %>%
  mutate(as.Date(Date))

# Filter for aapl closing price to get implied vol on these contracts
closing_price= aapl_price %>%
  filter(Date==(target_date)-30) %>%
  pull(Close)
closing_price

# Price euro calls and plot price surface and compare BSM versus simulated
strike_sim= c(avg_strike_chain$Strike)

BSM_call= function(S0,K,r,sigma,T){
  d1= (log(S0/K)+(r+0.5*sigma^2)*t)/(sigma*sqrt(t))
  d2= d1-sigma*sqrt(t)
  call_price= S0*pnorm(d1)-K*exp(-r*t)*pnorm(d2)
  return(call_price)
}

euro_call_sim=function(S0,K,r,sigma,t,nsteps,M,seed=100){
  set.seed(seed)
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
test= euro_call_sim(S0=closing_price,K=K,r=0.025, sigma=sigma,t=1/12,nsteps=30,M=10000)

option_chain= data.frame(
  Strike= strike_sim,
  BSM.Price= NA,
  Simulated.Price= NA
)

# Loop through strikes and store results for BSM
for (i in seq_along(strike_sim)){
  K= strike_sim[i]
  option_chain$BSM.Price[i]= round(BSM_call(S0=closing_price,K=K,r=0.025,sigma=my.stats[2,],T=1/12),2)
  option_chain$Simulated.Price[i]= round(euro_call_sim(S0=closing_price,K=K,r=0.025, sigma=my.stats[2,]-0.5,t=1/12,nsteps=50,M=10000),2)
}

# Stack Overflow helped here
wide_df= data.frame(
  strike = strike_sim,
  Market      = option_chain$BSM.Price,
  Simulation  = option_chain$Simulated.Price
)


long_df= pivot_longer(wide_df, -strike, names_to = "chain", values_to = "price")



# Plot the option price surfaces
ggplot(long_df, aes(x=strike, y=price,color=chain))+
  geom_line(size=1)+
  geom_point(size=2,alpha=0.7)+
  labs(
    title= "Option Price Chains: Black-Scholes vs. Simulation (-50 BPS)",
    x="Strike",
    y="Option Price",
    color="Chain"
  )+
  theme_minimal(base_size=14)

mse= mean( (wide_df$Market - wide_df$Simulation)^2 )
print(mse)
