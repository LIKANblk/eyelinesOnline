get_params <- function(eegTp, eegNTp, ufeats, A1_ch, A2_ch, sRate)
{
  library(sperrorest)
  
  nChannels <- dim(eegTp)[2] - 1
  bigT <- dim(eegTp)[1]
  N0 = dim(eegNTp)[3]
  N1 = dim(eegTp)[3]
  X0 <- matrix(nrow = N0, ncol = dim(ufeats)[1])
  X1 <- matrix(nrow = N1, ncol = dim(ufeats)[1])
  
  th_opt <- numeric()
  sens_tr <- numeric()
  spec_tr <- numeric()
  acc_tr <- numeric()
  sens_tst <- numeric()
  spec_tst <- numeric()
  acc_tst <- numeric()
  
  params <- list()
  acc <- list()
  spec <- list()
  sens <- list()
  
  
  for (i in 1:(dim(ufeats)[1]))
  {
    t <- ufeats[i, 1];
    ch <- ufeats[i, 2];
    X0[, i] <- eegNTp[t, ch, ]
    X1[, i] <- eegTp[t, ch, ]   
  }
  
  X <- rbind(X0,X1)
  Y <- c(rep(1, N0), rep(2, N1))
  CV <- partition.cv(as.data.frame(Y), nfold = 5)[[1]]
  W <- array(dim = c(nrow(ufeats),1,5))
  
  for (i in 1: length(CV))
  {
    trIdx <-  CV[[i]]$train
    tstIdx <-  CV[[i]]$test
    Xtr <- X[trIdx,]
    Xtst <- X[tstIdx,]
    Ytr <- Y[trIdx]
    Ytst <- Y[tstIdx]
    
    N0tr <- sum(Ytr == 1)
    N1tr <- sum(Ytr == 2)
    N0tst <- sum(Ytst == 1)
    N1tst <- sum(Ytst == 2)
    
    obj <- train_shrinkage(Xtr, Ytr)
    W[,,i] <- obj$W
    
    # calc threshold using all sample
    Q <- drop(X%*%W[,,i])
    Q0 <- Q[Y == 1] #non target
    Q1 <- Q[Y == 2] #target
    ths <- Q + eps()
    ths <- sort(ths)
    sens_all <- numeric()
    spec_all <- numeric()
    for (k in 1:length(ths))
    {
      sens_all[k] <- length(which(Q1 <= ths[k])) / N1
      spec_all[k] <- length(which(Q0 > ths[k])) / N0 
    }
    
    idx = which(spec_all >= 0.95)
    th_opt[i] = ths[idx[length(idx)]]
    
    Q <- Xtr%*%W[,,i]    
    Q0 <- Q[Ytr == 1]
    Q1 <- Q[Ytr == 2]
    sens_tr[i] <- length(which(Q1 <= th_opt[i])) / N1tr
    spec_tr[i] <- length(which(Q0 > th_opt[i])) / N0tr   
    acc_tr[i] <- (sens_tr[i] * N1tr + spec_tr[i] * N0tr) / (N1tr + N0tr)
    
    
    # test
    Q <- Xtst%*%W[,,i]    
    Q0 <- Q[Ytst == 1]
    Q1 <- Q[Ytst == 2]
    sens_tst[i] <- length(which(Q1 <= th_opt[i])) / N1tst
    spec_tst[i] <- length(which(Q0 > th_opt[i])) / N0tst    
    acc_tst[i] <- (sens_tst[i] * N1tst + spec_tst[i] * N0tst) / (N1tst + N0tst)
  }
  
  spec$tr = c(mean(spec_tr), std(spec_tr))
  sens$tr = c(mean(sens_tr), std(sens_tr))
  acc$tr = c(mean(acc_tr), std(acc_tr))
  spec$tst = c(mean(spec_tst), std(spec_tst))
  sens$tst = c(mean(sens_tst), std(sens_tst))
  acc$tst = c(mean(acc_tst), std(acc_tst))
  
  params$W = rowMeans(drop(W))
  params$th = mean(th_opt)
  params$feats = ufeats
  params$A1_ch = A1_ch
  params$A2_ch = A2_ch
  params$sRate = sRate
  
  p <- list(params = params, spec = spec, sens = sens, acc = acc)
  return(p)
  
}