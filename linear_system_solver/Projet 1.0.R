#import functions from scripts
source("C:/Users/Nicolas/Documents/EFREI/Cours/M1/Cours/Numerical Optimization Methods/Optimisation/linear_system_solver/Slacks.R")
source("C:/Users/Nicolas/Documents/EFREI/Cours/M1/Cours/Numerical Optimization Methods/Optimisation/linear_system_solver/Simplex.R")
source("C:/Users/Nicolas/Documents/EFREI/Cours/M1/Cours/Numerical Optimization Methods/Optimisation/linear_system_solver/2-Phase Simplex.R")
source("C:/Users/Nicolas/Documents/EFREI/Cours/M1/Cours/Numerical Optimization Methods/Optimisation/linear_system_solver/Split.R")
source("C:/Users/Nicolas/Documents/EFREI/Cours/M1/Cours/Numerical Optimization Methods/Optimisation/linear_system_solver/Cut.R")
source("C:/Users/Nicolas/Documents/EFREI/Cours/M1/Cours/Numerical Optimization Methods/Optimisation/linear_system_solver/IntToBin.R")
source("C:/Users/Nicolas/Documents/EFREI/Cours/M1/Cours/Numerical Optimization Methods/Optimisation/linear_system_solver/Binary.R")

#data import
library("readxl")

data1 <- read_excel("C:/Users/Nicolas/Documents/EFREI/Cours/M1/Cours/Numerical Optimization Methods/Optimisation/linear_system_solver/LP_System.xlsx")
data2 <- read_excel("C:/Users/Nicolas/Documents/EFREI/Cours/M1/Cours/Numerical Optimization Methods/Optimisation/linear_system_solver/LP_System2.xlsx")

main <- function(data){
  print("SOLVER")
  print("1. Full")
  print("2. Integral")
  print("3. Binary")
  
  choice <- readline(prompt="Your choice is : ")
  
  if (choice == 1){
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
      return(Two_Phase_Simplex(data_transformed))
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

