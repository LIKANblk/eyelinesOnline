train_classifier <- function(eegT, eegNT, fixationDuration, sRate, path, epoch_size, A1_ch, A2_ch, bsln_start, bsln_end, left_border, high)
{
  
  library(pracma)

  eegTp <- array(dim = c(dim(eegT)[1]/20, dim(eegT)[2], dim(eegT)[3]))
  eegNTp <- array(dim = c(dim(eegNT)[1]/20, dim(eegNT)[2], dim(eegNT)[3]))
  eegTp_before_dec <- array(dim = c(dim(eegT)[1], dim(eegT)[2], dim(eegT)[3]))
  eegNTp_before_dec <- array(dim = c(dim(eegNT)[1], dim(eegNT)[2], dim(eegNT)[3]))
  
  left_border <- left_border / 1000 * sRate
  bsln_start <- bsln_start / 1000 * sRate - left_border
  bsln_end <- bsln_end / 1000 * sRate - left_border
  
  
  bsln_start = max(bsln_start, 1)
  bsln_end = min(bsln_end, dim(eegT)[1])
  
  
  for (i in 1:dim(eegT)[3])
  {
    l <- eye_preprocess(eegT[,,i], bsln_start, bsln_end)
    eegTp[ , , i] <- l$eeg
    eegTp_before_dec[ , , i] <- l$before_dec
  }
  
  
  for (i in 1:dim(eegNT)[3])
  {
    l <- eye_preprocess(eegNT[,,i], bsln_start, bsln_end)
    eegNTp[ , , i] <- l$eeg
    eegNTp_before_dec[ , , i] <- l$before_dec    
  }
  
  spec_sens <- eye_1Dfeats(eegTp, eegNTp)
  
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