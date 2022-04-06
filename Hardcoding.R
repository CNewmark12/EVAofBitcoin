#Hardcoding my work
#There has been some funny behavior with my functions so I am hardcoding this
sorted <- sort(ClosePrice_BTC.US)
len_sorted <- length(sorted)
Hard_Hk <- c(1:(len_sorted-1))
for(k in 1:(len_sorted-1)){
  sum_er <- c(1:k)
  for(j in 1:k){
    sum_er[j] <- log((sorted[len_sorted+1-j]/sorted[len_sorted - k]))
  #log((sorted[len_sorted+1-j]/sorted[len_sorted - k]))
  }
  #print(k)
  Hard_Hk[k] <- mean(sum_er)
}

plot(Hard_Hk,type='l', xlim = c(0,1000), ylim = c(0,5))
plot(1/Hard_Hk, type ='l')
#Looks good here
######################################################################################################################################
#Now for the beta estimation
sorted <- sort(ClosePrice_BTC.US)
len_sorted <- length(sorted)
Hard_Hk <- c(1:(len_sorted-1))

kappa_1 <- round(sorted/10)
kappa_2 <- round(sorted/2)

Hard_Hk <- c(1:length(kappa_1:kappa_2))


for(k in kappa_1:kappa_2){
  sum_er <- c(1:k)
  for(j in 1:k){
    sum_er[j] <- log((sorted[len_sorted+1-j]/sorted[len_sorted - k]))
    #log((sorted[len_sorted+1-j]/sorted[len_sorted - k]))
  }
  #print(k)
  Hard_Hk[k] <- mean(sum_er)
}


H <- matrix(Hard_Hk)

Z <- cbind(matrix(c(rep(1,length(kappa_1:kappa_2)))),matrix(c(kappa_1:kappa_2)))

W <- diag(c(sqrt(kappa_1:kappa_2)),length(kappa_1:kappa_2),length(kappa_1:kappa_2))

betas <- solve((t(Z)%*%t(W)%*%W%*%Z))%*%t(Z)%*%t(W)%*%W%*%H

Sol <- list("betas" = betas, "H_k" = H_k)
