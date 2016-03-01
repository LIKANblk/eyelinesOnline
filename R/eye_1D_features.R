eye_1Dfeats <- function(eegTp, eegNTp, left_border, sRate)
{
  N0 <- dim(eegNTp)[3]
  N1 <- dim(eegTp)[3]
  
  nChannels <- dim(eegTp)[2] - 1
  t <- dim(eegTp)[1]
  
  beg_time <- left_border / 1000
  
  times_beg <- seq(0.3,0.45,0.02);
  times_end <- times_beg + 0.05;
  ts_beg <- round((times_beg - beg_time) * sRate);
  ts_end <- round((times_end - beg_time) * sRate);
  
  #   spec1 <- matrix(data = 0, nrow = t, ncol = nChannels) 
  #   sens1 <- matrix(data = 0, nrow = t, ncol = nChannels)
  
  eegTfilt <- eegTp;
  eegNTfilt <- eegNTp;
  
  X0 <- x <- matrix(nrow = N0, ncol = length(f_channels)*length(ts_beg))
  X1 <- x <- matrix(nrow = N1, ncol = length(f_channels)*length(ts_beg))
  
  #TMP!!!!
  f_channels <- c(7,8,9,10,11,12,13,14,15,16,17,18,19);
  
  for (i in 1:N1) 
  {
    x <- matrix(nrow = length(ts_beg), ncol = length(f_channels))
    for (t in 1:length(ts_beg))
    {
      x[t,] <- colMeans( eegTfilt[ts_beg[t]:ts_end[t], f_channels, i] )
    }
    X1[i,] <- as.vector(x)
  }
  
  for (i in 1:N0) 
  {
    x <- matrix(nrow = length(ts_beg), ncol = length(f_channels))
    for (t in 1:length(ts_beg))
    {
      x[t,] <- colMeans( eegNTfilt[ts_beg[t]:ts_end[t], f_channels, i] )
    }
    X0[i,] <- as.vector(x)
  }
  
  ###ended up here translating Trof matlab code
  
  
#   for(tt in 1:t)
#   {
#     for(ch in 1:nChannels)
#     {
#       X0 = eegNTp[tt, ch, ] 
#       X1 = eegTp[tt, ch, ]
#       
#       X = c(X0, X1)
#       Y = c(rep(1,N0),  rep(2,N1))
#       
#       obj = train_shrinkage(X,Y)
#       W <- obj$W
#       
#       Q0 <- W*X0
#       Q1 <- W*X1
#       ths <- c(Q0, Q1) + eps()
#       ths <- sort(ths) 
#       
#       spec <- numeric()
#       sens <- numeric()
#       
#       for (k in 2:length(ths))
#       {
#         sens[k] <- length(which(Q1 <= ths[k])) / N1
#         spec[k] <- length(which(Q0 > ths[k])) / N0
#         
#         if(spec[k] < 0.95)
#         {
#           idx <- k - 1
#           break
#         }
#       }
#       
#       ##########################!!!!!!!!!!!!!!
#       
#       spec1[tt, ch] <- spec[idx]
#       sens1[tt, ch] <- sens[idx]
#       
#     }
#   }
#   
#   ss <- list(spec1 = spec1, sens1 = sens1)
#   
#   return(ss)
}