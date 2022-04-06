#Coding POT for wave height formula to apply to return levels.
setwd("/Users/Chris Newmark/Documents/Academics/Research/Master's Project/Data") 
#data <- read.csv("bitcoinity_data_dailyPrices_2016to2018.csv",header=TRUE)
#data <- read.csv("coindesk_bitcoinData.csv",header=TRUE)

#"Coles(2001) argued that the interpretation of extreme value models
#in terms of return levels are usually more convenient than using individual parameters."

#Equation (7)
#This is the value exceeded on average once every m observations given the GPD model
#N: the number of events exceeding threshold u
#k: period of time k for your data
#u: threshold
#sigma: scale parameter
#xi: shape parameter
#ARI: average recurrence interval
returnLevel_m <- function(u,sigma,xi,ARI,N,k){
  return( u + (sigma/xi)*( (ARI* (N/k) )^xi -1 ) )
}

#Average recurrence interval: rank your data from highest to lowest
#then your time period + 1 / rank tells you how much time between
#occurrences on average to view that magnitude of event.
#lower ranked occurences are more rare and thus have a larger ARI
ARI <- function(time_period,ranking){
  return((time_period+1)/ranking)
}


#Equation (4)
#this is the value exceeded on average once every m observations (days)
#u: threshold
#sigma: scale parameter
#xi: shape parameter
#N: number of events exceeding threshold
#N_t: total number of data
#m: average number of observations (days) before we see the value
returnLevel_m2 <- function(u,sigma,xi,m,N,N_t){
  return( u + (sigma/xi)*( (m* (N/N_t) )^(xi) -1 ) )
}




