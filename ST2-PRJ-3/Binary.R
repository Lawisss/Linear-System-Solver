#script for binary function

Binary <- function(data){
  n <- nrow(data)
  m <- ncol(data)
  tree <- data.frame(matrix(0, ncol=(m-1),nrow=2^(m-2)))
  a<-2^(m-2)/2
  pos<-a
  # All nodes
  for (j in 1:((m-1)-1))
  {
    for (i in 1:(2^(m-2)))
    {
      if(i>pos){
        pos<-pos+2*a
      }
      if((i>(pos-a)&&(i<=pos)))
      {
        tree[i,j]=1
      }
      else
      {
        tree[i,j]=0
      }
    }
    a<-a/2
    pos<-a
  }
  # Calculate max for each node
  for (i in 1:(2^(m-2))) 
  {
    temp<-0
    for(j in 1:(m-2))
    {
      temp<-temp+data[n,j]*tree[i,j]
    }
    tree[i,m-1]=temp
  }
  # Find Optimum
  while(max(tree[,m-1])>=0)
  {
    optimum=max(tree[,m-1])
    for(i in 1:(2^(m-2)))
    {
      if(tree[i,m-1]==optimum)
      {
        node=tree[i,]
        pos = i
      }
    }
    if(check_constraints(data,node)==TRUE)
    {
      colnames(node)[ncol(node)]<-paste('MAX')
      return(node)
    }
    else
    {
      tree[pos,m-1]=-1
    }
  }
  return(tree)
}

check_constraints <- function(data,node){
  n <- nrow(data)
  m <- ncol(data)
  for (i in 1:(n-1)) 
  {
    temp<-0
    for(j in 1:(m-2))
    {
      temp<-temp+data[i,j]*node[1,j]
    }
    if(data[i,m-1]=='infeq')
    {
      if(temp <= data[i,m])
      {
        
      }
      else
      {
        return(FALSE)
      }
    }
    else if(data[i,m-1]=='supeq')
    {
      if(temp >= data[i,m])
      {
        
      }
      else
      {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}