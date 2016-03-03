train_classifier <- function(eegT, eegNT, fixationDuration, sRate, path, epoch_size, A1_ch, A2_ch, bsln_start, bsln_end, left_border, high)
{
  
  eegTp <- array(dim = c(dim(eegT)[1], dim(eegT)[2], dim(eegT)[3]))
  eegNTp <- array(dim = c(dim(eegNT)[1], dim(eegNT)[2], dim(eegNT)[3]))
  
  bsln_start = (bsln_start - left_border) / 1000 * sRate;
  bsln_end = (bsln_end - left_border) / 1000 * sRate;
  
  #   bsln_start = max(bsln_start, 1)
  bsln_end = min(bsln_end, dim(eegT)[1])
  
  
  for (i in 1:dim(eegT)[3])
  {
    eeg_baseline_corrected <- eye_preprocess(eegT[,,i], bsln_start, bsln_end)
    eegTp[ , , i] <- eeg_baseline_corrected
  }
  
  
  for (i in 1:dim(eegNT)[3])
  {
    eeg_baseline_corrected <- eye_preprocess(eegNT[,,i], bsln_start, bsln_end)
    eegNTp[ , , i] <- eeg_baseline_corrected
  }
  
  X0_X1 <- eye_1Dfeats(eegTp, eegNTp, left_border, sRate)
  X0 <- X0_X1$X0
  X1 <- X0_X1$X1
  
  #training
  
  nfold = 5
  
  N0 <-  dim(X0)[1]
  N1 <- dim(X1)[1]
  
  X <- rbind(X0, X1)
  Y <- c( rep(1, N0), rep(2, N1) )
  
  trIdxES <- vector("list", nfold)
  tstIdxES <- vector("list", nfold)
  
  CV <- createFolds(Y, nfold)
  
  #preallocate
  W <- matrix(ncol = nfold, nrow = dim(X1)[2] )
  th_opt <- vector(mode = "numeric", length = nfold)
  
  #generate random samples for training and testing
  for (i in 1:nfold) {
    trIdxES[[i]] <- sample( c( rep(0, length(CV[[i]])),
                               rep(1,  length(Y) - length(CV[[i]]) )
    ))
    tstIdxES[[i]] <- as.numeric(!trIdxES[[i]])
  }
  
  for (i in 1:length(CV)) 
  {
    trIdx <- trIdxES[[i]]
    tstIdx <- tstIdxES[[i]]
    Xtr <- X[which(trIdx==1), ]
    Xtst <- X[which(tstIdx==1), ]
    Ytr <- Y[which(trIdx==1)]
    Ytst <- Y[which(tstIdx==1)]
    N0tr <- sum(Ytr == 1)
    N1tr <- sum(Ytr == 2)
    N0tst <- sum(Ytr == 1)
    N1tst <- sum(Ytr == 2) 
    
    #train
    obj <- train_shrinkage(Xtr, Ytr)
    W[,i] <- obj$W
    
    #calc threshold using all sample
    Q <- X %*% W[,i]
    Q0 <- Q[which(Y==1)] #non target
    Q1 <- Q[which(Y==2)] #target
    ths <- Q + .Machine$double.eps
    ths <- sort(ths)
    
    sens_all <- vector()
    spec_all <- vector()
    
    for (k in 1:length(ths))
    {
      sens_all[k] <- length(which(Q1 <= ths[k])) / N1
      spec_all[k] <- length(which(Q0 > ths[k])) / N0
    }
    idx <- which(spec_all >= 0.95)
    idx <- idx[length(idx)]
    
    th_opt[i] <- ths[idx]
    #optimal operating point of the ROC curve
    roc_obj <- roc(c( rep(1,N1), rep(0, N0) ), c(Q1,Q0))
    roc_x <- roc_obj$specificities
    roc_y <- roc_obj$sensitivities
    
    ####to_do - ROC coordinates are different in R and in matlab!!!!!!!!! WHYYY!!!
    
    #auc calculation stolen from matlab
    auc <- 0.5 * sum(  ( roc_x[2:length(roc_x)] - roc_x[1:(length(roc_x)-1)] ) * ( roc_y[2:length(roc_y)]  + roc_y[1:(length(roc_y)-1)]))
  }
  
  Nf <- 110
  
  f_channels <- 1:10
  
  f_times <- 1:dim(eegTp)[1]
  
  tmp <- meshgrid(f_times, f_channels)
  
  ufeats <- matrix(nrow = length(tmp[[1]]), ncol = 2)
  ufeats[,1] <- as.numeric(tmp[[1]])
  ufeats[,2] <- as.numeric(tmp[[2]])
  
  pars <- get_params(eegTp, eegNTp, ufeats, A1_ch, A2_ch, fixationDuration, sRate)
  pars$params$bsln_start <- bsln_start
  pars$params$bsln_end <- bsln_end
  
  cat('Number of features: ', dim(ufeats)[1], '\n')
  cat('Train:', '\n')
  cat(sprintf(' Specificity: %.2f +- %.2f\n', pars$spec$tr[1], pars$spec$tr[2], '\n'))
  cat(sprintf(' Sensitivity: %.2f +- %.2f\n', pars$sens$tr[1], pars$sens$tr[2], '\n'))
  cat('Test:', '\n')
  cat(sprintf(' Specificity: %.2f +- %.2f\n', pars$spec$tst[1], pars$spec$tst[2], '\n'))
  cat(sprintf(' Sensitivity: %.2f +- %.2f\n', pars$sens$tst[1], pars$sens$tst[2], '\n'))
  
  return(pars$params)
  
}