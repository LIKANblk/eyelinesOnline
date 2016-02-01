train_shrinkage <- function(X,Y)
{
  nclasses <- length(unique(Y))
  if( is.null(dim(X)) )
  {
    xx <- matrix(nrow = length(X), ncol = 1)
    xx[,1] <- X
    X <- xx
  }
  
  d <- dim(X)[2]
  
  obj <- list()
  
  # estimate class priors
  obj$pi <- rep(0, nclasses)
  for (j in 1:nclasses)
  {
    obj$pi[j] = sum(Y==j)/length(Y)
  }
  
  # estimate class-conditional means
  obj$mu <- list()
  for (k in 1:nclasses)
  {
    if(dim(X)[2]==1)
    {
      obj$mu[[k]] = mean( X[Y==k], na.rm = T)
    }
    else
    {
      obj$mu[[k]] = colMeans( X[which(Y==k), ], na.rm = T)
    }
  }
  
  #estimate class-conditional covariance matrices
  
  obj$Sigma <- list()
  obj$lambda <- rep(0, nclasses)
  
  for (k in 1:nclasses)
  {
    obj$Sigma[[k]] <- var(X[which(Y == k), ])
    
    
    
    if (obj$lambda[k] == 0)
    {
      
      if(dim(X)[2]==1)
      {
        W <- numeric()
        mX <- (X[Y == k]) - (obj$mu[[k]])
        N <- length(mX)
        for (n in 1:N)
        {
          W[n] <- mX[n] * mX[n] 
        }
        WM <- mean(W)
        S <- (N / (N-1) ) * WM
        VS <- ( N/((N-1)^3) ) * sum(bsxfun("-",W,WM)^2) 
        v <- S
        t <- 0
        
        obj$lambda[k] <- sum(VS) / ( 2 * sum(t^2) + sum((S-v)^2) )
        
      }
      else
      {
        W <- array( dim = c(length(which(Y == k)), dim(X)[2], dim(X)[2]) )
        mX <- sweep((X[which(Y == k), ]), MARGIN = 2, (obj$mu[[k]]))
        N <- dim(mX)[1]
        for (n in 1:N)
        {
          matr <- replicate(length(mX[n,]), mX[n,]) 
          vect <- mX[n,]
          res <- matrix(nrow = length(vect), ncol = length(vect))
          for(z in 1:length(vect))
          {
            res[,z] <- matr[,z] * vect[z]
          }
          W[n,,] <- res
        }
        WM <- array(dim = c(1, dim(W)[3], dim(W)[3]))
        WM[1,,] <- apply(W, c(2,3), mean)
        
        S <- drop(WM * (N / (N-1) ))
        VS <- ( N/((N-1)^3) ) * apply( sweep(W, 2:3, drop(WM))^2 , 2:3, sum)
        v <- mean(diag(S))
        t <- triu(S,1)
        obj$lambda[k] <- sum(VS) / ( 2 * sum(t^2) + sum((diag(S)-v)^2) )
        
      }
      obj$lambda[k] <- max(0, min(1, obj$lambda[k]))
    }
    
    #the regularizing matrix
    bigT <- v*diag(d)
    obj$Sigma[[k]] <- (1- obj$lambda[k]) * obj$Sigma[[k]] + obj$lambda[k] * bigT
  }
  
  #compute inverse of joint covariance matrix (i.e. LDA instead of QDA)
  S <- 0
  for (c in 1:length(obj$Sigma))
  {
    S <- S + obj$Sigma[[c]]
  }
  
  obj$Sinv <- inv(S/length(obj$Sigma))
  obj$W <- drop(obj$Sinv%*%(obj$mu[[1]]-obj$mu[[2]]))
  
  return(obj)
  
}