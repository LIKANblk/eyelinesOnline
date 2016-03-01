eye_preprocess <- function(eeg, bsln_start, bsln_end)
{
  t <- dim(eeg)[1];
  
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

return(eeg_baseline_corrected)
  
}

