# params <- buildClassifier(path, fixation_threshold=500, epoch_size=1000, 
#                           left_border=-500, channels=1:17, A1=16, A2=17, 
#                           low=F, high=30, bsln_start = 200, bsln_end = 300, sRate = 500)
# path <- '/home/mayenok/Yandex.Disk/eyelinesOnline/data/test-3/'
# file_r2e <- "/home/mayenok/Yandex.Disk/eyelinesOnline/data/test-3/01.r2e"
# file_edf <- "/home/mayenok/Yandex.Disk/eyelinesOnline/data/test-3/24261813"

apply_new_classifier <- function(file_edf, file_r2e, params,
                                 epoch_size=1000, 
                                 left_border=-500, channels=1:15, A1=16, A2=17, 
                                 low=F, high=30, bsln_start = 200, bsln_end = 300, sRate = 500) {
  
  
  actions <- extract.actions(file_edf)
  signal = load.eeg(file_r2e, channels, low, high, c(A1,A2))
  sync_marks = which(signal[,dim(signal)[2]] != 0)
  signal <- signal[(sync_marks[3]+1):dim(signal)[1],]
  l <- prepare.data(signal, actions, epoch_size, sRate, left_border)
  
  eegT <- l$eegT
  eegNT <- l$eegNT
  
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
  
  feats <- makeFeatures(eegTp,  eegNTp, left_border, sRate)
  
  res1 <- feats$X1 %*% params$W < params$th
  res0 <- feats$X0 %*% params$W < params$th
  
  sens_tst <- sum(res1) / length(res1)
  spec_tst <- sum(res0 == F) / length(res0)
  
  auc_tst <- calc_roc_auc(dim(feats$X1)[1], dim(feats$X0)[1], feats$X1 %*% params$W, feats$X0 %*% params$W)
  
  cat("Sensitivity:", sens_tst, "\n")
  cat("Specificity:", spec_tst, "\n")
  cat("AUC:", auc_tst , "\n")
  
  
  list(res1, res0)
  
}