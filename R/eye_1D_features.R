eye_1Dfeats <- function(eegTp, eegNTp)
{
  nChannels = dim(eegTp)[2] - 1
  t = dim(eegTp)[1]
  N0 = dim(eegNTp)[3]
  N1 = dim(eegTp)[3]
  
  spec1 <- matrix(data = 0, nrow = t, ncol = nChannels) 
  sens1 <- matrix(data = 0, nrow = t, ncol = nChannels)
  
  for(tt in 1:t)
  {
    for(ch in 1:nChannels)
    {
      X0 = eegNTp[tt, ch, ] 
      X1 = eegTp[tt, ch, ]
      
      X = c(X0, X1)
      Y = c(rep(1,N0),  rep(2,N1))
      
      obj = train_shrinkage(X,Y)
      W <- obj$W
      
      Q0 <- W*X0
      Q1 <- W*X1
      ths <- c(Q0, Q1) + eps()
      ths <- sort(ths) 
      
      spec <- numeric()
      sens <- numeric()
      
      for (k in 2:length(ths))
      {
        sens[k] <- length(which(Q1 <= ths[k])) / N1
        spec[k] <- length(which(Q0 > ths[k])) / N0
        
        if(spec[k] < 0.95)
        {
          idx <- k - 1
          break
        }
      }
      
      ##########################!!!!!!!!!!!!!!
      
      spec1[tt, ch] <- spec[idx]
      sens1[tt, ch] <- sens[idx]
      
    }
  }
  
  ss <- list(spec1 = spec1, sens1 = sens1)
  
  return(ss)
}