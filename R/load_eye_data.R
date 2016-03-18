load_eye_data <- function(path, fixation_threshold, epoch_size, left_border,
                         sRate, channels, A1,A2, low, high)
{
  
  #setwd(path)
  
  json <- fromJSON(file = paste0(path,"meta.json"))
  
  epoch_size = epoch_size - left_border
  eegT = c()
  eegNT = c()
  
#   sink(sprintf("%soutput_%sHz.txt",path, high, high))
  
  for (i in 1:length(json$'valid files'))
  {
    actions <- extract.actions(paste0(path, json$'valid files'[[i]]$name_edf))
    #     signal = prepare(bigFatMatrix[[i]])
    signal = load.eeg(paste0(path, json$'valid files'[[i]]$name_eeg), channels, low, high, c(A1,A2))
    sync_marks = which(signal[,dim(signal)[2]] != 0)
    signal <- signal[(sync_marks[3]+1):dim(signal)[1],]
    
    #lines <- load.edf(sprintf("%s.edf", paste0(path, json$'valid files'[[i]]$name_edf)))
    #ans <- extract.samples(lines)
    #fixationDuration <- ans[[3]]
    
    #if(fixation_threshold != fixationDuration)
    #{
    #  next
    #}
    
    l <- prepare.data(signal, actions, epoch_size, sRate, left_border)
    
    eegT <- abind(eegT, l$eegT)
    eegNT <- abind(eegNT, l$eegNT)
    
  }
  
  
  l <- list(eegT = eegT,
            eegNT = eegNT,
            fixationDuration = fixation_threshold,
            sRate = sRate,
            path = path,
            epoch_size = epoch_size)  
  
  return(l)
}


load.eeg <- function(file, channels, low, high, refs){
  
  raw <- R3:::extractChannel(file, 0)
  #raw <- readMat(paste0(file, ".raw.mat"))$raw

  src <- source.channels(raw, 500);
  cl <- signalPreparation(src, low = low, high=high, notch=50, refs=refs, channels=channels) 
  
  
  f <- cl
  f <- cbind(f, raw[,33])
  f[,ncol(f)] = 0
  f[which(diff(raw[,33])>0), ncol(f)]=1
  f
  
}