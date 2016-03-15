train_classifier <- function(eegT, eegNT, fixationDuration, sRate, path,
                             epoch_size, A1_ch, A2_ch, bsln_start,
                             bsln_end, left_border, high)
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
  
  ans <- eye_train1(X0, X1, nfold)
  ans$fixDur <- fixationDuration
  ans$sRate <- sRate
  ans$feats <- X0_X1
  ans
  
}