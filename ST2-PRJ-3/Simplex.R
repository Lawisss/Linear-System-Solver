#script for Simplex function

Simplex <- function(data){
  pivotcolumn = which.max(data[nrow(data),])
  end = FALSE
  while(end==FALSE)
  {
    coltmp<-array()
    i=1
    while(i < nrow(data)+1){
      if((data[i,ncol(data)]/data[i,pivotcolumn])>0)
      {
        coltmp[i] = data[i,ncol(data)]/data[i,pivotcolumn]
      }
      i=i+1
    }
    pivotline = which.min(coltmp)
    data[pivotline,] <- data[pivotline,] / data[pivotline,pivotcolumn]
    n <- 1
    while (n < ncol(data)-1)
    {
      if(n==pivotline)
      {
        n <- n+1
      }
      else
      {
        data[n, ] <- data[n, ] - data[pivotline, ] * data[n,pivotcolumn]
        n <- n+1
      }
    }
    pivotcolumn = which.max(data[nrow(data),])
    end = TRUE
    for (i in 1:ncol(data))
    {
      if(data[nrow(data),i]>0)
      {
        end = FALSE
      }
    }
  }
  return(data)
}