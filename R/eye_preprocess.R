eye_preprocess <- function(eeg, bsln_start, bsln_end)
{
  t <- dim(eeg)[1];
  
  # baseline mean subtraction
  
  mean_baseline <- colMeans(eeg[bsln_start:bsln_end, ])
  e1 <- t(apply(eeg, 1, function(x) x - mean_baseline))
  before_dec <- e1 
  eeg_baseline_corrected <- e1

return(eeg_baseline_corrected)
  
}

