train_classifier <- function(eegT, eegNT, eegNT_test, sRate, path,
                             epoch_size, A1_ch, A2_ch, bsln_start,
                             bsln_end, left_border, high, channels, times_seq, decimation_window)
{
  
  eegTp <- array(dim = c(dim(eegT)[1], dim(eegT)[2], dim(eegT)[3]))
  eegNTp <- array(dim = c(dim(eegNT)[1], dim(eegNT)[2], dim(eegNT)[3]))
  eegNTp_test <- array(dim = c(dim(eegNT_test)[1], dim(eegNT_test)[2], dim(eegNT_test)[3]))
  
  bsln_start = (bsln_start - left_border) / 1000 * sRate;
  bsln_end = (bsln_end - left_border) / 1000 * sRate;
  
  #   bsln_start = max(bsln_start, 1)
  bsln_end = min(bsln_end, dim(eegT)[1])
  
  for (i in 1:dim(eegT)[3])
  {
    eegTp[ , , i] <- eye_preprocess(eegT[,,i], bsln_start, bsln_end)
  }
  
  
  for (i in 1:dim(eegNT)[3])
  {
    eegNTp[ , , i] <- eye_preprocess(eegNT[,,i], bsln_start, bsln_end)
  }
  
  for (i in 1:dim(eegNTp_test)[3])
  {
    eegNTp_test[ , , i] <- eye_preprocess(eegNT_test[,,i], bsln_start, bsln_end)
  }
  
  l <- makeFeatures(eegTp, eegNTp, eegNTp_test, left_border, sRate, times_seq, decimation_window)
  
  #training
  
  nfold = 5
  
  ans <- eye_train1(l$X0, l$X1, l$X_test, nfold)
  ans$sRate <- sRate
  ans
  
}