###################################################
# path - full path to folder containing .edf's, .r2e's and meta.txt (ending with "/")
# fixation_threshold - actual fixation threshold in training games
# left_border, epoch_size - left and right borders of classifier epoch
# channels - numbers of channels used in experiment
# A1, A2 - idex numbers of reference electrodes !in channels list!
# low, high - filters
###################################################
buildClassifier <- function(path, fixation_threshold=500, epoch_size=1000, 
                            left_border=-500, channels=1:17, A1=16, A2=17, 
                            low=F, high=30, bsln_start = 700, bsln_end = 800, sRate = 500)
  {
  #channels from Trofimov's clf = c('PZ','P3','P4','P1','P2','PO7','PO8','PO3','PO4','Oz','O1','O2','POz')
  l <- load_eye_data(path, fixation_threshold, epoch_size, left_border, sRate,
                    channels, A1,A2, low, high)
  
  process_params(l, channels, A1,A2, low, high, bsln_start, bsln_end, left_border)
}