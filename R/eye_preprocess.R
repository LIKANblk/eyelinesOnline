eye_preprocess <- function(eeg, bsln_start, bsln_end)
{
  t <- dim(eeg)[1];
  nChannels <- dim(eeg)[2];
  
  # montage
  #    ref <- (eeg[, A2_ch] + eeg[, A1_ch]) / 2
  #    e1 <- eeg - replicate(nChannels, ref)
  #    e1[, A2_ch ] <- -ref
  #    eeg <- e1;
  
  #   # centering
  #   base <- colMeans(eeg)
  #   s <- apply(eeg,2, sd)
  #   e1 <- eeg - t(replicate(t, base)) / t(replicate(t, s))
  #   eeg <- e1
  
  # baseline mean subtraction
  
  mean_baseline <- colMeans(eeg[bsln_start:bsln_end, ])
  e1 <- t(apply(eeg, 1, function(x) x - mean_baseline))
  before_dec <- e1 
  eeg_baseline_corrected <- e1
  
  
#   # smoothing
#   dec_n <- 20;
#   samples_in_epoch <- dim(eeg)[1]
#   e1 <- matrix(nrow = samples_in_epoch/dec_n, ncol = nChannels)
#   
#   for (i in 1:nChannels)
#   {
#     e1[ , i] <- decimate(eeg[ , i], dec_n)
#   }
#   
#   eeg <- e1
  
#   l <- list(eeg = eeg, before_dec = before_dec)

eeg_baseline_corrected
  
}

