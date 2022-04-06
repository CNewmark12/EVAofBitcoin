#Determining the GPD Threshold
setwd("/Users/Chris Newmark/Documents/Academics/Research/Master's Project/Data") 
#data <- read.csv("coindesk_bitcoinData.csv",header=TRUE)

#This will create the MRL Plot which is an exploratory technique used to help determine the 
#best threshold of the GPD model.

#We have n_u observations over the threshold u.
#These observations are x_1,...x_(n_u). Take x_max = max(x_1,...x_(n_u))
#MRL Plot
#
#
#
###############################################################
#Given a data set, return the values over a certain numeric threshold
data_over_threshold <- function(threshold,data){
  if(!is.na(plyr::empty(data[data > threshold])))
  return(data[data > threshold])
  else print("no data above threshold")
}

#Average the distances of the data from the threshold
MRL_Sum <- function(threshold,data){
  #first get the data over the threshold, then perform the average
  print(mean(data_over_threshold(threshold,data))-threshold)
  return(mean(data_over_threshold(threshold,data))-threshold)
}

#This returns the maximum value in a dataset
Data_Max <- function(threshold,data){
  return(max(data_over_threshold(threshold,data)))
}

#This creates a data point by pairing the threshold with its average distance from the data points
#as long as the threshold is less than the max value
MRL_Formula <- function(threshold,data){

  if(threshold < Data_Max(threshold,data)) 
    return(MRL_Sum(threshold,data))
}

#Thresholds will be a vector which iterates through potential thresholds for the data
MRL_Plot <- function(thresholds,data,plot=c('yes','no')){
  
  #create the means vector
  mrl.plot <- c(length(thresholds))
  
  #iterate through the thresholds and store their means from the data
  counter <- 1
  for (value in thresholds){
  mrl.plot[counter] <- MRL_Formula(value,data)
  counter <- counter + 1
  print(value)
  }
  
  #plot the thresholds against their means from the data
  if(plot == 'yes')
  plot(thresholds,mrl.plot)
  
  return(mrl.plot)
}







