###################################################
# path - full path to folder containing .edf's, .r2e's and meta.txt (ending with "/")
# fixation_threshold - actual fixation threshold in training games
# left_border, epoch_size - left and right borders of classifier epoch
# channels - numbers of channels used in experiment
# A1, A2 - idex numbers of reference electrodes !in channels list!
# low, high - filters
###################################################
buildClassifier <- function(path, epoch_size=1000, 
                            left_border=-500, channels = c(1:5,7,9:21), A1=22, A2=23, 
                            low=F, high=30, bsln_start = 200, bsln_end = 300,
                            sRate = 500, times_seq = seq(300,450, 20), decimation_window = 50)
  {
  #channels from Trofimov's clf = c('PZ','P3','P4','P1','P2','PO7','PO8','PO3','PO4','Oz','O1','O2','POz')
  
  # !!!TMP!!!
#   f_channels <- c(7,8,9,10,11,12,13,14,15,16,17,18,19);
  # !!!TMP!!!
  

  l <- load_eye_data(path, epoch_size, left_border, sRate,
                    channels, A1,A2, low, high)
  
  ret <- process_params(l, channels, A1,A2, low, high, bsln_start, bsln_end, left_border, times_seq, decimation_window)
  
  class(ret) <- 'eyelinesOnline_eeg_classifier'
  
  ret
}


print.eyelinesOnline_eeg_classifier <- function(x){
  
  con <- file()
  res <- x
  dump('res', file=con)
  classStr <- paste(readLines(con), sep = '', collapse='\n')
  
  on.exit(close(con))
  
  cat(sprintf(
    'library(Resonance)
library(Resonate)
library(eyelinesOnline)
    
res <- %s

online_epoch_start <- min(res$bsln_start, res$times_seq[1])
online_epoch_end <- max(res$bsln_end, (res$times_seq + res$decimation_window))
    
refs <- c(res$A1, res$A2)

online_epoch_size <- online_epoch_end - online_epoch_start
online_epoch_shift <- online_epoch_start - 500

times_seq <- res$times_seq - online_epoch_start

bsln_start <- res$bsln_start - online_epoch_start
bsln_end <- res$bsln_end - online_epoch_start

process = function(){

FS <- signalPreparation(input(1), low=res$low, high=res$high, notch=50, refs=refs, channels=res$channels)
ev <- perform_time_correction(input(1), input(2))
RA2 <- cross.windowizeByEvents(FS, ev, online_epoch_size/1000*SI(FS)$samplingRate, shift=online_epoch_shift/1000*SI(FS)$samplingRate)
RA3 <- pipe.medianWindow(RA2, (bsln_start)/1000* SI(RA2)$samplingRate, (bsln_end)/1000* SI(RA2)$samplingRate)
RA4 <- pipe.trof.classifier2(RA3, res$W, res$th, times_seq/1000, res$decimation_window/1000)
createOutput(RA4,"RES")
}', classStr
  ))
  
  invisible(x)
}