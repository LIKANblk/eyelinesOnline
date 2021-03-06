load_eye_data <- function(path, epoch_size, left_border,
                          sRate, channels, A1,A2, low, high,
                          no_button_press, random_non_target, ball_only,
                          n_random_nontarget)
{
  
  #setwd(path)
  
  json <- fromJSON(file = file.path(path,"meta.json"))
  
  eegT <- c()
  eegNT <- c()
  eegNT_test <- c()
  
  #   sink(sprintf("%soutput_%sHz.txt",path, high, high))
  if(n_random_nontarget)
  {
    n_train = 0
    for(i in 1:length(json$'files'))
    {
      if(json$'files'[[i]]$record_type == 'train') {
        n_train = n_train + 1
      }
    }
    n_random_nontarget = ceil( n_random_nontarget / n_train)
  } 
  
  for (i in 1:length(json$'files'))
  {
    if(json$'files'[[i]]$record_type == "train"){
      actions <- get_eye_actions(paste0(path, json$'files'[[i]]$name_edf))
      #     signal = prepare(bigFatMatrix[[i]])
      signal = load.eeg(paste0(path, json$'files'[[i]]$name_eeg), channels, low, high, c(A1,A2))
      sync_marks = which(signal[,dim(signal)[2]] != 0)
      signal <- signal[(sync_marks[3]+1):dim(signal)[1],]
      
      
      l <- prepare.data(signal, actions, epoch_size, sRate, left_border, no_button_press,
                        random_non_target, ball_only, n_random_nontarget, 
                        list(
                          edf = paste0(path, json$'files'[[i]]$name_edf),
                          r2e = paste0(path, json$'files'[[i]]$name_eeg)
                        )
                        )
      
      eegT <- abind(eegT, l$eegT)
      eegNT <- abind(eegNT, l$eegNT)
      eegNT_test <- abind(eegNT_test, l$eegNT_test)
    }
  }
  
  
  l <- list(eegT = eegT,
            eegNT = eegNT,
            eegNT_test = eegNT_test,
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
  f[which(diff(bitwAnd(raw[,33],2))==2)+1, ncol(f)]=1
  f
  
}