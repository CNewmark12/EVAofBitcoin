#This code will analyze the data for my master's project

#Setting working drive and reading in data
#The data contains daily prices for 2 years across many different online exchanges
setwd("/Users/Chris Newmark/Documents/Academics/Research/Master's Project/Data") 
#data <- read.csv("bitcoinity_data_dailyPrices_2016to2018.csv",header=TRUE)
data <- read.csv("coindesk_bitcoinData.csv",header=TRUE)



#Summary statistics on the average price(averaging the prices from the different exchanges) 
#and setting the variable length
######
#This is for bitcoinity_data
# AvgPrice <- data$Average
# summary(AvgPrice)
# n <- length(AvgPrice)
######

#This is for coin desk data
###
#####

#Set the working data dates
data_to_2016 <- data[as.Date(as.character(data$Date),"%m/%d/%Y") <= as.Date("08-26-2016", "%m-%d-%Y"),]
data_past_2016 <- data[as.Date(as.character(data$Date),"%m/%d/%Y") > as.Date("08-26-2016", "%m-%d-%Y"),]

#set working data variable, STATIC GLOBAL
workingData <- data_to_2016

#clean the data, set to working date up to 2016
ClosePrice <- workingData$Close.Price[!is.na(workingData$Close.Price)]
CloseDate <- workingData$Date[!is.na(workingData$Close.Price)]

###################################################################################################################################
#We may want the bitcoins per $1 US...this would give decimal bitcoins.
ClosePrice_BTC.US <- 1/ClosePrice
ClosePrice_BTC.US <- ClosePrice[!is.na(ClosePrice)]

###################################################################################################################################
#Set the length of the price vector
len <- length(ClosePrice)

norm.Returns <- array(dim=len)
for(i in 1:len-1){norm.Returns[i] = (ClosePrice[i+1] - ClosePrice[i])/ClosePrice[i]}

#Removing the NA from returns
norm.Returns <- norm.Returns[!is.na(norm.Returns)]

Rt_norm.Returns <- norm.Returns[norm.Returns >= 0]
Lt_norm.Returns <- norm.Returns[norm.Returns <= 0]

###################################################################################################################################
#Summary statistics on the returns, the mean and standard deviation of the returns
summary(norm.Returns)
hist(norm.Returns, breaks = 100)
sd(norm.Returns)
tail(sort(norm.Returns),150) #give me the 150 largest returns
mu_bar <- mean(norm.Returns, na.rm = TRUE)
sigma_sq <- var(norm.Returns, na.rm = TRUE)
sigma <- sd(norm.Returns, na.rm = TRUE)

###################################################################################################################################
#kurtosis of simulated normal distribution
library(e1071)
kurtosis(rnorm(5000)) #should be 3 or 0 depending on defintion

#Testing for normality
library(e1071)
kurtosis(norm.Returns)

skewness(rnorm(5000))
skewness(norm.Returns)

library(normtest)
jb.norm.test(norm.Returns,nrepl = 5000)
###################################################################################################################################

#fitting the left tail returns with the GPD using MLE
n <- 0.9 #set to 90%
require(fExtremes)
y <- gpdFit(-Lt_norm.Returns, u = quantile(-Lt_norm.Returns,n), type = 'mle', information = 'observed')
summary(y)


#Fit the negative returns to the 90% quantile value = kappa_1, plotting the distribution
plot(1:1000,dgpd(1:1000,0.4252792,mu = quantile(-Lt_norm.Returns,0.9),16.0460452),type='l')


###################################################################################################################################
#All the plots
#Calculating the MRL Plot from Ost
library(evir)
meplot(-Lt_norm.Returns, ylim = c(0,0.1),type='l', main='MEP Plot for Negative Returns')
meplot(norm.Returns, ylim = c(0,0.4),type='l', main='MEP Plot for Returns')

#This is the hill plot of the negative left returns with the associated k-values from [Zh]
library(evmix)
hp <- hillplot(-Lt_norm.Returns, ylim = c(0,5), legend.loc = NULL)
plot(1/hp$H,type='l',xlab = "Number of Observations,k", ylab = "Hill Estimator", main = 'Hill Plot of Negative Simple Returns')

points(k_star.loop2,1/hp$H[k_star.loop2],pch=19,col='red') #Associated k values from [Zh] algorithm

WLS.params <- Beta_Parameter_Estimation(sort(-Lt_norm.Returns),length(Lt_norm.Returns)) #Calculate the WLS regression parameters 
plot(1/hp$H)
abline(WLS.params$betas[1],WLS.params$betas[2],col="Red")
model_line <- (-0.001781757*(1:length(hp$H)) + 1.841568843)
e_k <- 1/hp$H - model_line
plot(e_k,type='l',xlab = "Number of Observations", ylab = "Hill Estimator", main = 'Residual Plot with ')
kappa_1 <- round(length(e_k)/10) 
kappa_2 <- round(length(e_k)/2)
abline(v=kappa_1)
abline(v=kappa_2)
#text(5,3,labels = "y = -0.001781757x + 1.841568843")

#Normal Distribution over Returns
library(MASS)
fit <- fitdistr(c(norm.Returns), "normal")
class(fit)

para <- fit$estimate

hist(norm.Returns, prob = TRUE, breaks = 100,xlim = c(-0.6,0.6),main = 'Histogram of Returns',xlab='Normalized Return Value')
curve(dnorm(x, para[1], para[2]), col = 2, add = TRUE)

#Zoom into negative returns
hist(norm.Returns, prob = TRUE, breaks = 150,xlim = c(-0.4,-0.05),ylim = c(0,1),main='Left Tail of Returns',xlab='Percent Loss')
curve(dnorm(x, para[1], para[2]), col = 2, add = TRUE)

#Kernel Density over Returns
hist(norm.Returns, prob = TRUE, breaks = 100,xlim = c(min(norm.Returns),max(norm.Returns)))
xval <-seq(min(norm.Returns),max(norm.Returns),length=40) 
yval <-density(norm.Returns,bw=0.5) 
#yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(yval, col="blue", lwd=2)
#This is not quite working

#BITCOIN IS NOT NORMAL PLOTS
#Let's try fitting t-distribution and generalized hyperbolic distribution
library(MASS)
fitdistr(c(norm.Returns),'t')
class(fit)

para <- fit$estimate
hist(norm.Returns, prob = TRUE, breaks = 100,xlim = c(-0.6,0.6),main = 'Histogram of Returns',xlab='Normalized Return Value')
library(TDist)
curve(dt(x, para[3]), col = 2, add = TRUE)



#For the plots of the GPD
sequence <- seq(from=1, to=10, by=0.1)
library(fExtremes)
y1 <- dgpd(sequence, xi=0.25) #xi > 0
y2 <- dgpd(sequence, xi=-0.25) #xi < 0
y3 <- dgpd(sequence, xi=0) #xi = 0

# plot the first curve by calling plot() function
# First curve is plotted
plot(sequence, y1, type="l", col="blue", pch="o", lty=1, xlab = "Index", ylab="Density" )

# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(sequence, y2, col="red", pch="*",type='l')
lines(sequence, y2, col="red",lty=2)

# Add Third curve to the same plot by calling points() and lines()
# Use symbol '+' for points.
points(sequence, y3, col="dark red",pch="+",type='l')
lines(sequence, y3, col="dark red", lty=3)

# Adding a legend inside box at the location (2,40) in graph coordinates.
# Note that the order of plots are maintained in the vectors of attributes.
legend("topright",0.92,legend=c("xi > 0","xi < 0","xi = 0"), col=c("blue","red","black"),lty=c(1,1,1), ncol=1)
title(main="Generalized Pareto Distribution")



#for the results, plotting the different Pareto distribution densities over the negative left returns
hist(-Lt_norm.Returns,breaks = 100, xlab = 'Return Percentage', ylab = 'Density', main = 'Density of Losses with Fitted GPDs', ylim = c(0,80),xlim=c(0.02,0.2))

# plot the first curve by calling plot() function
# First curve is plotted
library(evmix)
sequence <- seq(from=thresh_choices[1], to=1, by=0.01)
y1 <- dgpd(sequence, u = thresh_choices[1], xi = xi[1], sigmau = beta[1]) #model 1 u = 48
points(sequence, y1, type="l", col="blue", pch="o", lty=1, xlab = "Index", ylab="Density" )
lines(sequence, y1, col="blue",lty=1)

# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
library(evmix)
sequence <- seq(from=thresh_choices[2], to=1, by=0.01)
y2 <- dgpd(sequence, u = thresh_choices[2], xi = xi[2], sigmau = beta[2]) #model 2 u = 93
points(sequence, y2, col="red", pch="*",type='l')
lines(sequence, y2, col="red",lty=2)

# Add Third curve to the same plot by calling points() and lines()
# Use symbol '+' for points.
library(evmix)
sequence <- seq(from=thresh_choices[3], to=1, by=0.01)
y3 <- dgpd(sequence, u = thresh_choices[3], xi = xi[3], sigmau = beta[3]) #model 3 u = 96
points(sequence, y3, col="dark red",pch="+",type='l')
lines(sequence, y3, col="dark red", lty=3)


library(evmix)
sequence <- seq(from=thresh_choices[4], to=1, by=0.01)
y4 <- dgpd(sequence, u = thresh_choices[4], xi = xi[4], sigmau = beta[4]) #model 4 u = 150
points(sequence, y4, col="green",pch="+",type='l')
lines(sequence, y4, col="green", lty=4)

# Adding a legend inside box at the location (2,40) in graph coordinates.
# Note that the order of plots are maintained in the vectors of attributes.
legend("topright",cex=0.75,legend=c("mu = 0.12195122","mu = 0.08491736","mu = 0.08333333", "mu = 0.05600000"), col=c("blue","red","dark red","green"),lty=c(1,1,1,1), ncol=1)

###################################################################################################################################
#Trying out the best threshold for left norm.Returns
lamda.loop2 <- seq(from = 0, to = 3, by = 0.01)
k_star.loop2 <- c(rep(0,length(lamda.loop2)))
#Doesn't need sorted data
for(i in 1:length(lamda.loop2))
  k_star.loop2[i] <- turning_pt(-Lt_norm.Returns[-Lt_norm.Returns > 0],lambda = (i/100)) #here lambda is the number of standard deviations

plot(lamda.loop2,k_star.loop2, xlab = "lambda", ylab = "Threshold Index",type = 'l',xlim = c(0,3))


###################################################################################################################################
#Show me the thresholds!
plyr::count(k_star.loop2)

###################################################################################################################################
library(fExtremes)
increm <- 8
threshs.loop <- c(1:(108-increm))
for(i in 1:(108 - increm))
  threshs.loop[i] <- sort(-Lt_norm.Returns,decreasing=TRUE)[i+increm]


#Looping through potential thresholds and fitting the pareto
beta.loop <- c(1:(108-increm))
xi.loop <- c(1:(108-increm))
llh.loop <- c(1:(108-increm))

for(i in 1:(108-increm)){
  y <- gpdFit(-Lt_norm.Returns, u = threshs.loop[i], type = 'mle', information = 'observed')
  
  xi.loop[i] <- attributes(y)$fit$par.ests[1]
  beta.loop[i] <- attributes(y)$fit$par.ests[2]
  llh.loop[i] <- attributes(y)$fit$llh
  #print(i)
}

#new_threshs <- threshs[threshs < threshs[7]]

#Plots of thresholds against the parameter values
plot(new_threshs,xi,type = 'l')
plot(new_threshs,beta,type = 'l')

#Plots of the parameters against how far the threshold is away from maximum value
plot(xi,type='l')
plot(beta,type='l')

###################################################################################################################################
#Results: 3 threshold choices! Let's fit them and the suggested one from Osterreider
sorted_norm.Returns <- sort(-Lt_norm.Returns, decreasing = TRUE)
thresh_choices <- c(sorted_norm.Returns[49],sorted_norm.Returns[94],sorted_norm.Returns[97],sorted_norm.Returns[151],sorted_norm.Returns[68],sorted_norm.Returns[109],sorted_norm.Returns[34])
###################################################################################################################################
beta <- c(1:7)
xi <- c(1:7)
nllh <- c(1:7)
library(fExtremes)
for(i in 1:7){
  y <- gpdFit(-Lt_norm.Returns, u = thresh_choices[i], type = 'mle', information = 'observed')
  
  xi[i] <- attributes(y)$fit$par.ests[1]
  beta[i] <- attributes(y)$fit$par.ests[2]
  nllh[i] <- attributes(y)$fit$llh[1]
  #print(i)
}
####################################################################################################################################

###################################################################################################################################
#quant <- ecdf(sort(-Lt_norm.Returns))
q_vector <- c(1:1077)
for(i in 1:1077) q_vector[i] <- 1-(i/1077)

var_vector <- c(1:1077)
for(i in 1:1077) var_vector <- VaR(sort(norm.Returns),q_vector[i])

q_48 <- 1 - (48/1077)
q_93 <- 1 - (93/1077)
q_96 <- 1 - (96/1077)
q_150 <- 1 - (150/1077)
#Calculating VaR and ES
library(PerformanceAnalytics)
VaR(sort(norm.Returns),q_48)
VaR(Lt_norm.Returns,q_48)
VaR(sort(Lt_norm.Returns),q_48)
VaR(sort(Lt_norm.Returns),q_93)
VaR(sort(Lt_norm.Returns),q_96)
VaR(sort(Lt_norm.Returns),q_150)

#######################################################################################################################################
#Calculaing m observation recurrence corresponding to the first pareto distribution model parameters
#not sure what "m" is here...
#sort(-Lt_norm.Returns,decreasing = TRUE)[10] #0.2315353 or 23.15353% loss
#ari <- ARI(2192,10) #expect about a 23% loss to occur once every 219 days
#returnLevel_m(thresh_choices[1],beta[1],xi[1],ari,48,2192)

#m = 5 means we are calculating the value we expect to exceed every 5 days on average
m_return <- 5
returnLevel_m2(u = thresh_choices[1],sigma = beta[1],xi = xi[1],m = m_return,N=48,N_t=2192)
returnLevel_m2(u = thresh_choices[2],sigma = beta[2],xi = xi[2],m = m_return,N=93,N_t=2192) 
returnLevel_m2(u = thresh_choices[3],sigma = beta[3],xi = xi[3],m = m_return,N=96,N_t=2192) 
returnLevel_m2(u = thresh_choices[4],sigma = beta[4],xi = xi[4],m = m_return,N=150,N_t=2192) 
returnLevel_m2(u = thresh_choices[5],sigma = beta[5],xi = xi[5],m = m_return,N=67,N_t=2192) 
returnLevel_m2(u = thresh_choices[6],sigma = beta[6],xi = xi[6],m = m_return,N=108,N_t=2192) 
returnLevel_m2(u = thresh_choices[7],sigma = beta[7],xi = xi[7],m = m_return,N=33,N_t=2192) 


#m = 20 means we are calculating the value we expect to exceed every 20 days on average
m_return <- 20
returnLevel_m2(u = thresh_choices[1],sigma = beta[1],xi = xi[1],m = m_return,N=48,N_t=2192) 
returnLevel_m2(u = thresh_choices[2],sigma = beta[2],xi = xi[2],m = m_return,N=93,N_t=2192) 
returnLevel_m2(u = thresh_choices[3],sigma = beta[3],xi = xi[3],m = m_return,N=96,N_t=2192) 
returnLevel_m2(u = thresh_choices[4],sigma = beta[4],xi = xi[4],m = m_return,N=150,N_t=2192) 
returnLevel_m2(u = thresh_choices[5],sigma = beta[5],xi = xi[5],m = m_return,N=67,N_t=2192) 
returnLevel_m2(u = thresh_choices[6],sigma = beta[6],xi = xi[6],m = m_return,N=108,N_t=2192) 
returnLevel_m2(u = thresh_choices[7],sigma = beta[7],xi = xi[7],m = m_return,N=33,N_t=2192) 


#m = 240 means we are calculating the value we expect to exceed every 240 days on average
m_return <- 240
returnLevel_m2(u = thresh_choices[1],sigma = beta[1],xi = xi[1],m = m_return,N=48,N_t=2192) 
returnLevel_m2(u = thresh_choices[2],sigma = beta[2],xi = xi[2],m = m_return,N=93,N_t=2192)
returnLevel_m2(u = thresh_choices[3],sigma = beta[3],xi = xi[3],m = m_return,N=96,N_t=2192) 
returnLevel_m2(u = thresh_choices[4],sigma = beta[4],xi = xi[4],m = m_return,N=150,N_t=2192)
returnLevel_m2(u = thresh_choices[5],sigma = beta[5],xi = xi[5],m = m_return,N=67,N_t=2192) 
returnLevel_m2(u = thresh_choices[6],sigma = beta[6],xi = xi[6],m = m_return,N=108,N_t=2192) 
returnLevel_m2(u = thresh_choices[7],sigma = beta[7],xi = xi[7],m = m_return,N=33,N_t=2192) 


#for a range of values we can loop through to see which thresholds give the worst 10day forcast! As a worst case scenario...
returnLevel_m.loop <- c(1:length(threshs.loop))
for(i in 1:length(threshs.loop)){
  returnLevel_m.loop[i] <- returnLevel_m2(u = threshs.loop[i],sigma = beta.loop[i],xi = xi.loop[i],m = 10,N=(i+7),N_t=2192)
}

plot(returnLevel_m.loop,type='l')
plot(llh.loop,type='l')

#Using the adhoc calculation from [Rd]
adhoc.kstar(-Lt_norm.Returns[-Lt_norm.Returns > 0],150,0.25)


#Calculating the empirical probability for a certain range vs. the predictions from the models
#P(X <= 50 | X > mu)

F_1(xi[1],thresh_choices[1],beta[1],0.20) #For model 1
F_1(xi[2],thresh_choices[2],beta[2],0.20) #For model 2
F_1(xi[3],thresh_choices[3],beta[3],0.20) #For model 3
F_1(xi[4],thresh_choices[4],beta[4],0.20) #For model 4
F_1(xi[5],thresh_choices[5],beta[5],0.20) #For model 5
F_1(xi[6],thresh_choices[6],beta[6],0.20) #For model 6
F_1(xi[7],thresh_choices[7],beta[7],0.20) #For model 7

F_1(xi[1],thresh_choices[1],beta[1],0.40) #For model 1
F_1(xi[2],thresh_choices[2],beta[2],0.40) #For model 2
F_1(xi[3],thresh_choices[3],beta[3],0.40) #For model 3
F_1(xi[4],thresh_choices[4],beta[4],0.40) #For model 4
F_1(xi[5],thresh_choices[5],beta[5],0.40) #For model 5
F_1(xi[6],thresh_choices[6],beta[6],0.40) #For model 6
F_1(xi[7],thresh_choices[7],beta[7],0.40) #For model 7

F_1(xi[1],thresh_choices[1],beta[1],0.30) #For model 1
F_1(xi[2],thresh_choices[2],beta[2],0.30) #For model 2
F_1(xi[3],thresh_choices[3],beta[3],0.30) #For model 3
F_1(xi[4],thresh_choices[4],beta[4],0.30) #For model 4
F_1(xi[5],thresh_choices[5],beta[5],0.30) #For model 5
F_1(xi[6],thresh_choices[6],beta[6],0.30) #For model 6
F_1(xi[7],thresh_choices[7],beta[7],0.30) #For model 7

#empirical probabilities
library(dplyr)
length(Lt_norm.Returns.comb[between(-Lt_norm.Returns.comb, thresh_choices[1], 0.2)]) / length(Lt_norm.Returns.comb[-Lt_norm.Returns.comb > thresh_choices[1]])
length(Lt_norm.Returns.comb[between(-Lt_norm.Returns.comb, thresh_choices[2], 0.2)]) / length(Lt_norm.Returns.comb[-Lt_norm.Returns.comb > thresh_choices[2]])
length(Lt_norm.Returns.comb[between(-Lt_norm.Returns.comb, thresh_choices[3], 0.2)]) / length(Lt_norm.Returns.comb[-Lt_norm.Returns.comb > thresh_choices[3]])
length(Lt_norm.Returns.comb[between(-Lt_norm.Returns.comb, thresh_choices[4], 0.2)]) / length(Lt_norm.Returns.comb[-Lt_norm.Returns.comb > thresh_choices[4]])
length(Lt_norm.Returns.comb[between(-Lt_norm.Returns.comb, thresh_choices[5], 0.2)]) / length(Lt_norm.Returns.comb[-Lt_norm.Returns.comb > thresh_choices[5]])
length(Lt_norm.Returns.comb[between(-Lt_norm.Returns.comb, thresh_choices[6], 0.2)]) / length(Lt_norm.Returns.comb[-Lt_norm.Returns.comb > thresh_choices[6]])
length(Lt_norm.Returns.comb[between(-Lt_norm.Returns.comb, thresh_choices[7], 0.2)]) / length(Lt_norm.Returns.comb[-Lt_norm.Returns.comb > thresh_choices[7]])

length(Lt_norm.Returns.comb[between(-Lt_norm.Returns.comb, thresh_choices[1], 0.3)]) / length(Lt_norm.Returns.comb[-Lt_norm.Returns.comb > thresh_choices[1]])
length(Lt_norm.Returns.comb[between(-Lt_norm.Returns.comb, thresh_choices[2], 0.3)]) / length(Lt_norm.Returns.comb[-Lt_norm.Returns.comb > thresh_choices[2]])
length(Lt_norm.Returns.comb[between(-Lt_norm.Returns.comb, thresh_choices[3], 0.3)]) / length(Lt_norm.Returns.comb[-Lt_norm.Returns.comb > thresh_choices[3]])
length(Lt_norm.Returns.comb[between(-Lt_norm.Returns.comb, thresh_choices[4], 0.3)]) / length(Lt_norm.Returns.comb[-Lt_norm.Returns.comb > thresh_choices[4]])
length(Lt_norm.Returns.comb[between(-Lt_norm.Returns.comb, thresh_choices[5], 0.3)]) / length(Lt_norm.Returns.comb[-Lt_norm.Returns.comb > thresh_choices[5]])
length(Lt_norm.Returns.comb[between(-Lt_norm.Returns.comb, thresh_choices[6], 0.3)]) / length(Lt_norm.Returns.comb[-Lt_norm.Returns.comb > thresh_choices[6]])
length(Lt_norm.Returns.comb[between(-Lt_norm.Returns.comb, thresh_choices[7], 0.3)]) / length(Lt_norm.Returns.comb[-Lt_norm.Returns.comb > thresh_choices[7]])

#Calculating the MSE for the predictions.
MSE <- c(rep(0,7))
MSE.calc <- c(rep(0,2))
percents <- c(0.2,0.3)
for(i in 1:7){
  for(j in 1:2){
    MSE.calc[j] <- (F_1(xi[i],thresh_choices[i],beta[i],percents[j]) - length(Lt_norm.Returns.comb[between(-Lt_norm.Returns.comb, thresh_choices[i], percents[j])]) / length(Lt_norm.Returns.comb[-Lt_norm.Returns.comb > thresh_choices[i]]))^2
  }
  MSE[i] <- mean(MSE.calc)
  MSE.calc <- c(rep(0,2))
}

Beta_Parameter_Estimation(-Lt_norm.Returns,length(-Lt_norm.Returns))
