
eye_train1 <- function(X0, X1, X_real_nontarget, nfold)
{
  #####################  preallocate  #########################
  W <- matrix(ncol = nfold, nrow = dim(X1)[2] )
  th_opt <- vector(mode = "numeric", length = nfold)
  
  auc0 <- vector(mode = "numeric", length = nfold)
  auc_tr <- vector(mode = "numeric", length = nfold)
  auc_tst <- vector(mode = "numeric", length = nfold)
  
  sens_tr <- vector(mode = "numeric", length = nfold)
  spec_tr <- vector(mode = "numeric", length = nfold)
  sens_tst <- vector(mode = "numeric", length = nfold)
  spec_tst <- vector(mode = "numeric", length = nfold)
  
  acc_tr <- vector(mode = "numeric", length = nfold)
  acc_tst <- vector(mode = "numeric", length = nfold)
  
  params <- list()
  spec <- list()
  sens <- list()
  acc <- list()
  auc <- list()
  
  trIdxES <- vector("list", nfold)
  tstIdxES <- vector("list", nfold)
  
  #############################################################
  
  N0 <-  dim(X0)[1]
  N1 <- dim(X1)[1]
  
  X <- rbind(X0, X1)
  Y <- c( rep(1, N0), rep(2, N1) )
  
  CV <- createFolds(Y, nfold)
  
  #generate random samples for training and testing
  
  for (i in 1:length(CV)) 
  {
    Xtr <- X[-CV[[i]], ] 
    Xtst <- X[CV[[i]], ] # только те которые реально были нецелевыми в эксперименте
    Ytr <- Y[-CV[[i]]]
    Ytst <- Y[CV[[i]]]
    N0tr <- sum(Ytr == 1)
    N1tr <- sum(Ytr == 2)
    N0tst <- sum(Ytst == 1)
    N1tst <- sum(Ytst == 2) 
    
    #train
    obj <- train_shrinkage(Xtr, Ytr)
    W[,i] <- obj$W
    
    #calc threshold using all sample
    # вместо X - X валидационный, часть тренировочной выборки, которая не пойдет в шринкаж. 
    Q <- X %*% W[,i]
    Q0 <- Q[which(Y==1)] #non target
    Q1 <- Q[which(Y==2)] #target
    ths <- Q + .Machine$double.eps
    ths <- sort(ths)
    
    spec_all <- vector()
    
    for (k in 1:length(ths))
    {
      spec_all[k] <- length(which(Q0 > ths[k])) / N0
    }
    idx <- which(spec_all >= 0.95)
    idx <- idx[length(idx)]
    
    th_opt[i] <- ths[idx]
    
    # FIRST TODO Now for estimating AUC we use whole data set, with Xtr - it's not correct, so here we have to use only test (achived from createFolds) indices
    #optimal operating point of the ROC curve
    total_Q <- rbind(X_real_nontarget, X1) %*% W[,i]
    auc0[i] <- calc_roc_auc(nrow(X1), nrow(X_real_nontarget), total_Q[-(1:nrow(X_real_nontarget))], total_Q[1:nrow(X_real_nontarget)])
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #auc0_old[i] <- calc_roc_auc(N1,N0,Q1,Q0)
    
    #calc acc on train sample
    Q <- Xtr %*% W[,i]
    Q0 <- Q[Ytr == 1]
    Q1 <- Q[Ytr == 2]
    
    tr_vals <- calc_acc(Q1, N1tr, Q0, N0tr, th_opt[i]) 
    acc_tr[i] <- tr_vals$acc
    sens_tr[i] <- tr_vals$sens
    spec_tr[i] <- tr_vals$spec
    auc_tr[i] <- calc_roc_auc(N1tr,N0tr,Q1,Q0)
    
    #test
    Q <- Xtst %*% W[,i]
    Q0 <- Q[Ytst == 1]
    Q1 <- Q[Ytst == 2]
    
    tst_vals <- calc_acc(Q1, N1tst, Q0, N0tst, th_opt[i])
    acc_tst[i] <- tst_vals$acc
    sens_tst[i] <- tst_vals$sens
    spec_tst[i] <- tst_vals$spec
    auc_tst[i] <- calc_roc_auc(N1tst,N0tst,Q1,Q0)
    
  }
  
  spec$tr = c(mean(spec_tr), std(spec_tr))
  sens$tr = c(mean(sens_tr), std(sens_tr))
  acc$tr = c(mean(acc_tr), std(acc_tr))
  spec$tst = c(mean(spec_tst), std(spec_tst))
  sens$tst = c(mean(sens_tst), std(sens_tst))
  acc$tst = c(mean(acc_tst), std(acc_tst))
  auc$tr = c(mean(auc_tr), std(auc_tr))
  auc$tst = c(mean(auc_tst), std(auc_tst))
  auc$all = c(mean(auc0), std(auc0))
  
  params$W = rowMeans(W)
  params$th = mean(th_opt)
  
  ans <- list(spec = spec,
              sens = sens,
              acc = acc,
              auc = auc,
              params = params)
  
}

#auc calculation stolen from matlab
auc_calc_matlab <- function(x,y) {
  auc <- 0.5 * sum(  ( x[2:length(x)] - x[1:(length(x)-1)] ) * ( y[2:length(y)]  + y[1:(length(y)-1)]))
  auc
}

calc_roc_auc <- function(N1,N0,Q1,Q0){
  roc_obj <- roc( c( rep(1, N1),  rep(0, N0) ), c(Q1, Q0))
  roc_x <- roc_obj$specificities
  roc_y <- 1 - roc_obj$sensitivities
  
  auc <- auc_calc_matlab(roc_x, roc_y)
  auc <- max(auc , 1 - auc)
  auc
}

calc_acc <- function(Q1, N1, Q0, N0, th_opt) {
  sens = length(which(Q1 <= th_opt)) / N1
  spec = length(which(Q0 > th_opt)) / N0
  acc = (sens  * N1 + spec  * N0) / (N1 + N0)
  list(spec = spec, sens = sens, acc = acc)
}