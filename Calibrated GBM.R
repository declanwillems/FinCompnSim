# Steen, Willems and Apostolico Financial Comp and Sim Project
# Now that I've match multiple stocks across simulations
# Calibrate the model for a single stock
rm(list=ls())

# Libraries
library("ggplot2")
library("moments")

# Get single stock price dataaa
ticker= "AAPL"
getSymbols(ticker, src="yahoo",from="2015-01-01")
Stock_price= AAPL[,6]

# Returns on single stock
R= diff(log(Stock_price),lag=1)
R= na.omit(R)



# Summarize the moments of the assets:
my.stats= as.dataa.frame(matrix(NA,nrow=4,ncol=ncol(R)))
names(my.stats)= names(R)
my.stats[1,]= apply(R,2,mean)
my.stats[2,]= apply(R,2,var)
my.stats[3,]= apply(R,2,skewness)
my.stats[4,]= apply(R,2,kurtosis)
print(my.stats,digits=5)

# Plot price distribution
tics= Stock_price
pts= list(NA)
n= 2

ggplot(R, aes(x = R[, 1])) + 
  geom_histogram(aes(y = ..density..),
                 bins = 100,
                 colour = "black", 
                 fill = "white") +
  stat_function(fun = dnorm,
                args = list(mean = my.stats[1, 1],
                            sd = sqrt(my.stats[2, 1]))) +
  ggtitle("Single Stock")

# Daily to annualized returns
dt= 1/252
mu= mean(R$AAPL.Adjusted)/dt # This doesn't seem right returns 19%
sigma= sd(R$AAPL.Adjusted)*sqrt(1/dt) 

# init price (According to class notes, this doesn't matter so set to 1)
X0= 1

# Number of simulations
n.sim= nrow(R) # This makes things easier later

# Simulate price paths
t= 1/252
stock_paths= X0*exp((mu - 0.5*sigma^2)*t+sigma*sqrt(t)*rnorm(n.sim))
stock_paths_R= log(stock_paths/X0) # log returns of stock paths
R$stock_sim= stock_paths_R
d= density(R$stock_sim)

# Stack results
M= nrow(R)
data= dataa.frame(
  xx= rbind(matrix(R$AAPL.Adjusted,ncol=1),
            matrix(R$stock_sim,ncol=1)),
  yy = rep(c("Stock","Sim"),each = M))
data

my.plot= ggplot(data,aes(x=xx)) + 
  geom_histogram(data=subset(data,yy == 'Stock'),aes(y =..density..),
                 bins=100,
                 colour = "black", 
                 fill = "blue",
                 alpha=0.5) +
  geom_histogram(data=subset(data,yy == 'Sim'),aes(y =..density..),
                 bins=100,
                 colour = "black", 
                 fill = "red",
                 alpha = 0.5) + 
  stat_function(fun = dnorm, args = list(mean = mu*dt, sd = sigma*sqrt(dt)))

print(my.plot)
