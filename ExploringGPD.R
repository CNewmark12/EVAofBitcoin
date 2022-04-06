
################################################################
beta_0 <- 0.025
eta_0 <- 0.247
mu_0 <- mu_bar
#Generalized Pareto Distribution - cumulative function
#Epsilon not equal to 0
F_1 <- function(eta,mu,beta,x){
  1-(1+(eta*(x-mu)/beta))^(-1/eta)
}
#This function looks like the survivor function! See 2007 EVA book pg 27 (1.9)

#Epsilon equal to 0
F_2 <- function(mu,beta,x){
  1-exp(-(x-mu)/beta)
}

#Exploring the shape of the Pareto Distribution
plot(0:2000,F_1(eta=2,mu=0,beta=1,seq(from = -1,to = 1, by = 0.001)),type="l")

plot(F_2(mu=0,beta=beta_0,seq(from = -1,to = 1, by = 0.001)),type="l")



###############################################################
#Generalized Pareto density function
###############################################################

plot(F_2(mu=mu_0,beta=beta_0,Returns),type="l")
plot(F_1(eta=eta_0,mu=mu_0,beta=beta_0,Returns),type="l")

library(fExtremes)
curve(dgpd(x,beta=0.025,xi=0.247,mu=0.022), xlab = '', ylab = 'Density', main = 'Generalized Pareto Distribution',
      xlim = c(0,0.2))
points(0.022,0,col = 'red',pch = 19) #we want this to be labelled.
#with(dgpd(x,beta=0.025,xi=0.247,mu=0.022),text(labels = 'Threshold',pos = 4))

