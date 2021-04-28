#Script for Integral
check_boundaries <- function(data){
  n <- nrow(data)
  m <- ncol(data)
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
        i<-i+1
      }
      else
      {
        i<-i+1
      }
    }
    if(stop==FALSE){
      return(FALSE)
    }
  }
  return(TRUE)
}