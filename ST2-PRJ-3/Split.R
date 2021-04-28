source("./Slacks.R")
source("./Simplex.R")
source("./2-Phase Simplex.R")

#Script for Split function
Split <- function(data){
  #Information to keep
  constraints=data[1:(nrow(data)-1),]
  objective_function=data[nrow(data),]
  nb_variables=ncol(data)-2
  
  #Slacks + Simplex
  data_calculated=Simplexs(data)
  
  if(data_calculated[nrow(data_calculated),ncol(data_calculated)]%%1==0)
  {
    return(data_calculated)
  }
  
  #Split every information in two to add the two new constraints
  constraints1=constraints
  constraints2=constraints
  data_calculated1=data_calculated
  data_calculated2=data_calculated
  #Add the constraints for each variable
  line1=objective_function
  line2=objective_function
  while(data[nrow(data),ncol(data)]%%1!=0){ #Do as follow until the max is an integral
  for(k in 1:nb_variables)
  {
    line1[k]=0
    line2[k]=0
  }
  line1[ncol(objective_function)-1]='infeq'
  line2[ncol(objective_function)-1]='supeq'
  line1[2]=1
  line2[2]=1
  for(m in 1:nrow(data_calculated1))
  {
    if(data_calculated1[m,2]==1)
    {
      line1[ncol(line1)]=floor(data_calculated1[m,ncol(data_calculated1)])
      line2[ncol(line2)]=floor(data_calculated2[m,ncol(data_calculated2)]+1)
    }
  }
  
  #New constraints
  constraints1=rbind(constraints1,line1)
  constraints2=rbind(constraints2,line2)
  
  #Data with new constraint
  data_split1=rbind(constraints1,objective_function)
  data_split2=rbind(constraints2,objective_function)
  
  #Simplex
  data_calculated1=Simplexs(data_split1)
  data_calculated2=Simplexs(data_split2)
  
  #If data_calculated optimum is integral, return result
  if((data_calculated1[nrow(data_calculated1),ncol(data_calculated1)]%%1==0) && (data_calculated2[nrow(data_calculated2),ncol(data_calculated2)]%%1==0))
  {
    if(data_calculated1[nrow(data_calculated1),ncol(data_calculated1)]>=data_calculated2[nrow(data_calculated2),ncol(data_calculated2)])
    {
      return(data_calculated1)
    }
    else
    {
      return(data_calculated2)
    }
  }
  else if(data_calculated1[nrow(data_calculated1),ncol(data_calculated1)]%%1==0)
  {
    return(data_calculated1)
  }
  else if(data_calculated2[nrow(data_calculated2),ncol(data_calculated2)]%%1==0)
  {
    return(data_calculated2)
  }
  #Take constraint from the higher optimum
  if(data_calculated1[nrow(data_calculated1),ncol(data_calculated1)]>=data_calculated2[nrow(data_calculated2),ncol(data_calculated2)])
  {
    constraints1=constraints1
    constraints2=constraints1
  }
  else
  {
    constraints1=constraints2
    constraints2=constraints2
  }
  return(data_calculated1)
  }
  #end of while
}
