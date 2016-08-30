perform_time_correction <- function(eeg, events){
  processor(
    eeg, events,
    prepare = function(env){
      env$synced <- FALSE
      env$syncstep <- 0
      env$correction <- 0

      env$signal <- matrix(0.0, ncol=SI(eeg)$channels, nrow=2^5)
      env$pointer <- 0L
      env$si.times <- vector(mode = "double", length=nrow(env$signal))
      env$lastTS <- NA
      env$lastSample <- 0
      
      SI(events)
    },
    online = function(eeg, events){
      if(synced){
        if(length(events)>0){
          split <- strsplit(sapply(events, as.character), ':')
          filt <- sapply(split, function(x) x[[1]]!='sync')
   
          return(       
            mapply(function(event, str){
              
              attr(event, 'TS') <- as.double(str[[4]])*1E6+correction
              
            } ,events[filt], split[filt])
          )
        } else {
          return(list())
        }
      }
      
      if(nrow(eeg)>0){
        if(nrow(eeg)+pointer >= nrow(signal))
        {
          tmp <- matrix(0.0, ncol=ncol(eeg), nrow=(nrow(signal)+nrow(eeg))*1.5)
          rowsCopy(tmp,0, signal, 0, -1)
          signal <<- tmp
          tmp <- vector(mode="double", length=nrow(signal))
          tmp[1:length(si.times)] <- si.times
          si.times <<- tmp
        }
        
        rowsCopy(signal, pointer, eeg, 0, -1)
        si.times[1:nrow(eeg)+pointer] <<- attr(eeg, 'TS')
        pointer <<- pointer+nrow(eeg)
      }
      
      if(length(events)>0){
        lapply(events, function(x){
          
          str <- strsplit(x, ':')[[1]]
          if(str[[1]]=='sync' && str[[2]]==' 2 '){
            syncstep <<- syncstep+1
            if(syncstep==3){
              correction <<- as.double(str[[3]])
            }
          }
          
        })
      }
      
      if(correction>0){
        ct <- which(diff(bitwAnd(signal[,33],2))==2)
        
        if(length(ct)>=3){
          synced <<- TRUE
          correction <<- si.times[ ct[[3]] ] - correction*1E3
        }
      }
      
      list()
    }
  )
}