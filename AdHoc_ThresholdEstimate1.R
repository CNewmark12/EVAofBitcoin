#The Ad Hoc procedure from [Reiss and Thomas] for estimating the number of extremes
setwd("/Users/Chris Newmark/Documents/Academics/Research/Master's Project/Data") 
data <- read.csv("coindesk_bitcoinData.csv",header=TRUE)

#Take estimates of the shape parameter from k to n.



adhoc.func <- function(data, k, beta.smooth){
  data.extremes <- sort(data)[(length(data)-(k+1)):length(data)] # k upper extremes
  len <- length(data.extremes)
  shape.estimates <- c(1:len)
  #first estimate the shape for each value
  for(i in len:1){
    library(fExtremes)
    possibleError <- tryCatch(gpdFit(data.extremes, u = data.extremes[i], type = 'mle', information = 'observed'), error=function(e) e)
    if(!inherits(possibleError, "error")){
    y <- gpdFit(data.extremes, u = data.extremes[i], type = 'mle', information = 'observed')
    shape.estimates[len-i+1] <- attributes(y)$fit$par.ests[1]
    }
    else{shape.estimates[len-i+1] <- 0} #not sure about this since the estimate still contributes to the median even though it was not calculated
  }
  #shape.estimates[i] represents the shape parameter based on the i upper extremes where i=1 would be based on 1 upper extreme etc.
  med.shape <- median(shape.estimates)
  shape.sum <- c(1:k)
  for(i in 1:k){
    shape.sum[i] <- (i^beta.smooth)*abs(shape.estimates[i] - med.shape)
  }
  return(mean(shape.sum))
}


adhoc.kstar <- function(data, k, beta.smooth){
  k.star <- 1
  minimize <- adhoc.func(data,1,beta.smooth)
  minimize.compare <- 0
  for(i in 2:k){
    minimize.compare <- adhoc.func(data,i,beta.smooth)
    # print(i)
    # print(minimize)
    # print(minimize.compare)
    if(minimize.compare <= minimize | minimize == 0){
      k.star <- i
      minimize <- minimize.compare
    }
  }
  return(k.star)
}

