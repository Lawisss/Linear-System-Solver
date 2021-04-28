#import functions from scripts
#getwd()
#setwd("C:/Users/mbmaj/OneDrive/Documents/S8/UE - ITF FUNDAMENTALS II/Numerical Optimization Methods/ST2NOM-PRJ-3")
source("./Slacks.R")
source("./Simplex.R")
source("./2-Phase Simplex.R")
source("./Split.R")
source("./Cut.R")
source("./IntToBin.R")
source("./Binary.R")
source("./Integral.R")

#data import
library("readxl")

data1 <- read_excel("./LP_System.xlsx")
data2 <- read_excel("./LP_System2.xlsx")
data3 <- read_excel("./LP_System3.xlsx")
data4 <- read_excel("./LP_System4.xlsx")
data5 <- read_excel("./LP_System5.xlsx")
data6 <- read_excel("./LP_System6.xlsx")
data7 <- read_excel("./LP_System7.xlsx")

main <- function(data){
  print("SOLVER")
  print("1. Full")
  print("2. Integral")
  print("3. Binary")
  
  choice <- readline(prompt="Your choice is : ")
  
  if (choice == 1)
  {
    Simplexs(data)
  }
  else if(choice == 2)
  {
    #Integral
    if(check_boundaries(data))
    {
      data <- IntToBin(data)
      print(data)
      return(Binary(data))
    }
    else
    {
      #Method
      print("METHOD")
      print("1. Split")
      print("2. Cutting Planes (not working)")
      
      choice <- readline(prompt="Your choice is : ")
      
      if(choice == 1)
      {
        #Split
        return(Split(data))
      }
      else if (choice == 2)
      {
        #Cutting Planes
        return(Cut(data))
      }
      else
      {
        print("Error: Wrong number.")
      }
    }
  }
  else if(choice == 3)
  {
    #Binary
    return(Binary(data))
  }
  else
  {
    print("Error: Wrong number.")
  }
}
