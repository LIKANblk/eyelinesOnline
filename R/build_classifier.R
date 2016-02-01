###################################################
# path - full path to folder containing .edf's, .r2e's and meta.txt (ending with "/")
# fixation_threshold - actual fixation threshold in training games
# left_border, epoch_size - left and right borders of classifier epoch
# channels - numbers of channels used in experiment
# A1, A2 - idex numbers of reference electrodes !in channels list!
# low, high - filters
###################################################
buildClassifier <- function(path, fixation_threshold=800, epoch_size=400, 
                            left_border=150, channels=1:17, A1=16, A2=17, 
                            low=0.1, high=30, bsln_start = 200, bsln_end = 300)
  {
  #@todo: move magick numbers to arguments
  l <- eye_loaddata(path, fixation_threshold, epoch_size, left_border, 500,
                    channels, A1,A2, low, high)
  
  read.par.mat(l, channels, A1,A2, low, high, bsln_start, bsln_end, left_border)
}