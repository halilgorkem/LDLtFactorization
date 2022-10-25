
A <- matrix(c(4,-1,1,
              -1,4.25,2.75,
              1,2.75,3.50), ncol = 3, nrow = 3, byrow = T)

LDLt <-  function(A)
{
  n <- nrow(A)
  L <- matrix(c(0), nrow = n, ncol = n )
  diag(L) <- 1
  D <- rep(0,n) #D <- matrix(c(0), n,n)
  D[1] <- A[1,1]
  L[,1] <- A[,1] / D[1]
  #Adım1
  for(i in 2:(n-1))
  {
    v <-c() #v <- rep(0,i-1)
    #Adım2
    v <- L[i,1:(i-1)]*D[1:(i-1)]
    
    #Adım3
    D[i] <- A[i,i] - sum(L[i,1:(i-1)]*v[1:(i-1)])
    
    #Adım4
    for(j in (i+1):n)
    {
      L[j,i] <- (A[j,i] - sum(L[j, 1:(i-1)] * v[1:(i-1)])) / D[i]
    }
  }
  v <- L[n, 1:(n-1)]*D[1:(n-1)]
  D[n] <- A[n,n]-sum(L[n, 1:(n-1)] * v[1:(n-1)])
  
  
  list(L=L,D=diag(D))
}
LDLt(A)
