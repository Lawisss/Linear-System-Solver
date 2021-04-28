source("./Slacks.R")

#script for cut function
Cut <- function(data){
  #Transform and Add Slacks
  data_transformed = slacks(data)
  
  return(data)
}
