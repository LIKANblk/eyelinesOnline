perform_time_correction <- function(eeg, events){
  processor(
    eeg, events,
    prepare = function(env){
      env$synced <- false
      env$syncstep <- 0
      env$correction <- 0

      env$signal <- matrix(0.0, ncol=SI(data)$channels, nrow=2^5)
      env$pointer <- 0L
      env$si.times <- vector(mode = "double", length=nrow(env$signal))
      env$lastTS <- NA
      env$lastSample <- 0
      
      SI(events)
    },
    online = function(eeg, events){
      if(synced){
        if(length(events)>0){
          split <- strsplit(events, ':')
          filt <- sapply(split, function(x) x[[1]]!='sync')
   
          return(       
            papply(function(event, str){
              
              attr(event, 'TS') <- as.double(str[[3]])*1E6+correction
              
            } ,events[[filt]], split[[filt]])
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
          if(str[[1]]=='sync' && str[[2]]==' 2'){
            syncstep <<- syncstep+1
            if(syncstep==3){
              correction <<- as.double(str[[3]])
            }
          }
          
        })
      }
      
      if(correction>0){
        ct <- which(diff(bitwAnd(eeg[,33],2))==2)
        
        if(length(ct)>=3){
          synced <<- TRUE
          correction <<- signal.ts[ ct[[3]] ] - correction*1E3
        }
      }
      
      list()
    }
  )
}