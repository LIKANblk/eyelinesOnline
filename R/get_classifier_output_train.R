get_classifier_output_train <- function(filename_r2e, start_epoch, end_epoch, times, dwell_time, res) {
  
  con <- file()
  dump('res', file=con)
  res_str <- paste(readLines(con), sep = '', collapse='\n')
  close(con)
  
  classifier <- sprintf('
  
library(Resonance)
library(Resonate)
library(eyelinesOnline)


%3$s

refs <- c(res$A1, res$A2)

process = function(){
  times <- input(3)
  raw_epoch <- cross.windowizeByEvents(input(1), times, %1$i/1000*SI(FS)$samplingRate, %2$i/1000*SI(FS)$samplingRate)
  createOutput(raw_epoch, "raw_epochs")
  FS2 <- signalPreparation(input(1), low=res$low, high=res$high, notch=50, refs=refs, channels=c(res$channels, 29, 30, 31, 32))
  M <- diag(nrow=SI(FS2)$channels-2, ncol=SI(FS2)$channels)
  M[SI(FS2)$channels-3, SI(FS2)$channels-2 ] = -1
  M[SI(FS2)$channels-2, SI(FS2)$channels -0:2   ] = c(-1,1,0)
  FS2 <- pipe.spatial(FS2, M)
  filtered_epochs <- cross.windowizeByEvents(FS2, times, %1$i/1000*SI(FS)$samplingRate, %2$i/1000*SI(FS)$samplingRate) 
  createOutput(filtered_epochs, "filtered_epochs")
}
 ', max(dwell_time)+end_epoch-start_epoch, start_epoch-max(dwell_time), res_str)
  
  data <- readStructurized(filename_r2e)
  stream <- Filter(function(x) x$name=='EEG', data$streams)[[1]]
  
  rawEEG <- do.call(merge, Filter(function(x){ identical(SI(x), stream) },  data$blocks))
  
  syncTS <- attr(rawEEG, 'TS')[ which(diff(bitwAnd(rawEEG[,33],2))==2)[3]  ]
  
  time_events <- source.events(rep('', length(times)), times*1E3+syncTS)
  
  data$streams[[3]] <- SI(time_events)
  data$streams[[3]]$id = 3
  data$streams[[3]]$name = 'custom_events'
  data$blocks <- c(data$blocks, lapply(time_events, function(x) {
    x <- list(x)
    SI(x) <- data$streams[[3]]
    class(x) <- 'DB.event'
    x
  }))
  
  cutExcess <- function(epochs, dwell_time){
    mapply(function(eeg, len){
      times <- seq(to=nrow(eeg), length=(len-start_epoch+end_epoch)/1000*SI(epochs)$samplingRate)
      ret <- eeg[times,]
      attr(ret, 'TS') <- attr(eeg, 'TS')[times]
      ret
    }, epochs, dwell_time[1:length(epochs)])
  }
  
  result <- run.offline(data$streams, data$blocks, classifier)
  
  raw_epochs <- cutExcess(result$raw_epochs, dwell_time)
  filtered_epochs <- cutExcess(result$filtered_epochs, dwell_time)
  
  list(
    raw_epochs = raw_epochs, 
    filtered_epochs = filtered_epochs, 
    sampling_rate = stream$samplingRate
  )
  
}