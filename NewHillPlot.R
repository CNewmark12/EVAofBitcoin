setwd("/Users/Chris Newmark/Documents/Academics/Research/Master's Project/Data") 
#data <- read.csv("coindesk_bitcoinData.csv",header=TRUE)

##############################################
#### This is for the basic hill plot
OrderData_Ascending <- function(data){
  return(sort(data))
}


#This is equation (1) in Zhou et al.
#index runs from 1,...,n-1
#In this case, k is considered the "threshold"
#data[n] will be the largest data point
#n is the length of the data
Hill_Estimator <- function(k,data,n){
  nMinusk <- (n-k)
  Hk <- c(1:k)
  
  
  for (j in 1:(k)){
    index <- (n+1-j)
    Hk[j] <- log(data[index]/data[nMinusk])
  }
  
  return(mean(Hk))
}

Hill_Plot_Reciprocal <- function(data){
  return(1/data)
}

Hill_Plot_Vector <- function(k,data,n){

  #we are expecting the data to be ordered
  #the length of the data is n
  return(Hill_Plot_Reciprocal(Hill_Estimator(k,data,n)))
}


#################################################
#################################################
#We will estimate the parameters of the linear model for the hill estimator
Beta_Parameter_Estimation <- function(data,n){
  
  kappa_1 <- round(n/10)
  kappa_2 <- round(n/2)

  H_k <- c(1:length(kappa_1:kappa_2))

  
  for(i in kappa_1:kappa_2)
  H_k[i-kappa_1+1] <- Hill_Estimator(i,data,n)
  
  
  
  H <- matrix(1/H_k) #WARNING I'm Changing this from H_k to 1/H_k to see if my parameters will fit my 1/H_k plot
  
  Z <- cbind(matrix(c(rep(1,length(kappa_1:kappa_2)))),matrix(c(kappa_1:kappa_2)))

  W <- diag(c(sqrt(kappa_1:kappa_2)),length(kappa_1:kappa_2),length(kappa_1:kappa_2))
  
  betas <- solve((t(Z)%*%t(W)%*%W%*%Z))%*%t(Z)%*%t(W)%*%W%*%H
  
  Sol <- list("betas" = betas, "H_k" = H_k)
  return(Sol)
} #beta[1] is beta_0 y intercept, beta[2] is beta_1 slope


#################################################
#################################################
#Choosing the turning point

#Doesn't need sorted data
turning_pt <- function(data,lambda){
  n <- length(data)
  kappa_1 <- round(n/10) 
  kappa_2 <- round(n/2)
  
  e_k <- c(1:kappa_2)
  
  Sol <- Beta_Parameter_Estimation(sort(data),length(data))
  
  beta_0 <- Sol$betas[1]
  beta_1 <- Sol$betas[2]
  
  H_k <- c(1:kappa_2)
  
  for(i in 1:kappa_2)
    H_k[i] <- Hill_Estimator(i,data,n)
  
  #step 1
  for(i in 1:kappa_2)
    e_k[i] <- H_k[i] - (beta_0 + beta_1*i)
  
  
  #step 2
  #This gives the standard deviation in the range of our kappas
  s_hat = sqrt((1/(kappa_2 - kappa_1))*sum(e_k[kappa_1:kappa_2]^2))

  #step 3
  
  k_s <- e_k[1:kappa_1]
  k_star <- 1
  
  #iterate through k_s, if that value matches my criteria, then store its index
  for(i in 1:kappa_1){
    
    if(abs(k_s[i]) >= (lambda*s_hat))
      k_star <- i
  }
  
  return(k_star)

}











