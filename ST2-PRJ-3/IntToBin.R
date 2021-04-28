#script for integral to binary function
IntToBin <- function(data){
  n <- nrow(data)
  m <- ncol(data)
  non_base <- data[0]
  base <- data[0]
  # Separation of base and non base line
  for(j in 1:(m-2))
  {
    i <- 1
    stop <- FALSE
    while(i<n && stop!=TRUE)
    {
      if(data[i,j]==1)
      {
        stop<-TRUE
        for(k in 1:(m-2))
        {
          if(k==j)
          {
            
          }
          else if(data[i,k]!=0)
          {
            stop=FALSE
          }
        }
        if(stop==FALSE)
        {
          non_base<-rbind(non_base,data[i,])
        }
        else
        {
          base<-rbind(base,data[i,])
        }
        i<-i+1
      }
      else
      {
        i<-i+1
      }
    }
  }
  for(i in 1:(n-1))
  {
    if(data[i,1]!=1)
    {
      if(data[i,1]==0)
      {
        add<-FALSE
        for(j in 1:(m-2))
        {
          if(data[i,1]!=0 & data[i,1]!=1)
          {
            add<-TRUE
          }
        }
        if(add==TRUE)
        {
          non_base<-rbind(non_base,data[i,]) 
        }
      }
      else
      {
        non_base<-rbind(non_base,data[i,]) 
      }
    }
  }
  non_base<-rbind(non_base,data[n,])
  # Transform from Integral to binary
  data_tranform<-data.frame(matrix(0, ncol=0,nrow=nrow(non_base)))
  for(i in 1:nrow(base))
  {
    count<-0
    power<-0
    while(2**power<=base[i,ncol(base)])
    {
      power<-power+1
    }
    new_data<-data.frame(matrix(0, ncol=power,nrow=1))
    for(j in 1:power)
    {
      new_data[1,j]<-2**(j-1)
    }
    data_to_add<-data.frame(matrix(0, ncol=power,nrow=nrow(non_base)))
    for(j in 1:nrow(data_to_add))
    {
      for(k in 1:ncol(data_to_add))
      {
        data_to_add[j,k]<-new_data[1,k]*non_base[j,i]
      }
      for(j in 1:ncol(data_to_add)){
        colnames(data_to_add)[j] <- paste("X",i,j)
      }
    }
    data_tranform<-cbind(data_tranform,data_to_add)
  }
  data_tranform<-cbind(data_tranform,non_base[,(ncol(non_base)-1):ncol(non_base)])
  return(data_tranform)
}