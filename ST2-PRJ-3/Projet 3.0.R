#import functions from scripts
#setwd("C:/Users/mbmaj/OneDrive/Documents/S8/UE - ITF FUNDAMENTALS II/Numerical Optimization Methods/ST2NOM-PRJ-3")
source("./Slacks.R")
source("./Simplex.R")
source("./2-Phase Simplex.R")
source("./Split.R")
source("./Cut.R")
source("./IntToBin.R")
source("./Binary.R")

#data import
library("readxl")

data1 <- read_excel("./LP_System.xlsx")
View(data1)

data2 <- read_excel("./LP_System2.xlsx")
View(data2)

main <- function(data){
  print("SOLVER\n")
  print("1. Full")
  print("2. Integral")
  print("3. Binary")
  
  choice <- readline(prompt="Your choice is : ")
  
  if (choice == 1){
    max = data[nrow(data),1:(ncol(data)-2)]
    print(max)
    #Transform and Add Slacks
    data_transformed = slacks(data)
    
    test <- FALSE
    i=1
    while(i<nrow(data_transformed) & test == FALSE){
      if(data_transformed[nrow(data_transformed)-i,ncol(data_transformed)-i]==-1)
      {
        test = TRUE
      }
      i=i+1
    }
    
    if(test == TRUE)
    {
      #2-Phase Simplex
      return(Two_Phase_Simplex(data_transformed,max))
    }
    else
    {
      #Simplex
      return(Simplex(data_transformed))
    }
  }
}

main(data1)
main(data2)


