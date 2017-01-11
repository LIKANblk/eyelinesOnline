get_classifier_output <- function(filename_r2e, filename_classifier, start_epoch, end_epoch, times, dwell_time) {
  classifier <- readChar(filename_classifier, file.info(filename_classifier)$size)
  classifier <- strRep(classifier, '\r', '')
  classifier <- strRep(classifier, 'pipe.trof.classifier2', 'pipe.trof.classifier.output')
  
  if(grepl('RA5', classifier)){
    
    classifier <- strRep(classifier, 'createOutput(RA5,"RES")',sprintf('
      createOutput(RA5,"RES")
      
      createOutput(RA4, "classifierOut")
      createOutput(input(2), "classifierIn")
      
      times <- input(4)
      raw_epoch <- cross.windowizeByEvents(input(1), times, %1$i/1000*SI(FS)$samplingRate, %2$i/1000*SI(FS)$samplingRate)
      createOutput(raw_epoch, "raw_epochs")
      FS2 <- signalPreparation(input(1), low=res$low, high=res$high, notch=50, refs=refs, channels=c(res$channels, 29, 30, 31, 32))
      M <- diag(nrow=SI(FS2)$channels-2, ncol=SI(FS2)$channels)
      M[SI(FS2)$channels-3, SI(FS2)$channels-2 ] = -1
      M[SI(FS2)$channels-2, SI(FS2)$channels -0:2   ] = c(-1,1,0)
      FS2 <- pipe.spatial(FS2, M)
      filtered_epochs <- cross.windowizeByEvents(FS2, times, %1$i/1000*SI(FS)$samplingRate, %2$i/1000*SI(FS)$samplingRate) 
      createOutput(filtered_epochs, "filtered_epochs")', max(dwell_time)+end_epoch-start_epoch, start_epoch-max(dwell_time)))
 
   } else {
    
    classifier <- strRep(classifier, 'createOutput(RA4,"RES")',sprintf('
      createOutput(RA4,"RES")
      
      createOutput(RA4, "classifierOut")
      createOutput(input(2), "classifierIn")
      
      times <- input(4)
      raw_epoch <- cross.windowizeByEvents(input(1), times, %1$i/1000*SI(FS)$samplingRate, %2$i/1000*SI(FS)$samplingRate)
      createOutput(raw_epoch, "raw_epochs")
      FS2 <- signalPreparation(input(1), low=res$low, high=res$high, notch=50, refs=refs, channels=c(res$channels, 29, 30, 31, 32))
      M <- diag(nrow=SI(FS2)$channels-2, ncol=SI(FS2)$channels)
      M[SI(FS2)$channels-3, SI(FS2)$channels-2 ] = -1
      M[SI(FS2)$channels-2, SI(FS2)$channels -0:2   ] = c(-1,1,0)
      FS2 <- pipe.spatial(FS2, M)
      filtered_epochs <- cross.windowizeByEvents(FS2, times, %1$i/1000*SI(FS)$samplingRate, %2$i/1000*SI(FS)$samplingRate) 
      createOutput(filtered_epochs, "filtered_epochs")', max(dwell_time)+end_epoch-start_epoch, start_epoch-max(dwell_time)))
  }
  
  data <- readStructurized(filename_r2e)
  
  stream <- Filter(function(x) x$name=='EEG', data$streams)[[1]]
  
  rawEEG <- do.call(merge, Filter(function(x){ identical(SI(x), stream) },  data$blocks))
  
  syncTS <- attr(rawEEG, 'TS')[ which(diff(bitwAnd(rawEEG[,33],2))==2)[3]  ]
  
  time_events <- source.events(rep('', length(times)), times*1E3+syncTS)
  
  data$streams[[4]] <- SI(time_events)
  data$streams[[4]]$id = 4
  data$streams[[4]]$name = 'custom_events'
  data$blocks <- c(data$blocks, lapply(time_events, function(x) {
    x <- list(x)
    SI(x) <- data$streams[[4]]
    class(x) <- 'DB.event'
    x
  }))
  
  cutExcess <- function(epochs, dwell_time){
    mapply(function(eeg, len){
      times <- seq(to=nrow(eeg), length=(len-start_epoch+end_epoch)/1000*SI(epochs)$samplingRate)
      ret <- eeg[times,]
      attr(ret, 'TS') <- attr(eeg, 'TS')[times]
      ret
    }, epochs, dwell_time[1:length(epochs)], SIMPLIFY = FALSE)
  }

  result <- run.offline(data$streams, data$blocks, classifier)
  
  result.DF <- do.call(rbind,result$RES)
  
  raw_epochs <- cutExcess(result$raw_epochs, dwell_time)
  filtered_epochs <- cutExcess(result$filtered_epochs, dwell_time)
  
  classifierIn <- do.call(rbind, result$classifierIn)
  classifierOut <- do.call(rbind, result$classifierOut)
  list(
    classifier_output = result.DF, 
    raw_epochs = raw_epochs, 
    filtered_epochs = filtered_epochs, 
    sampling_rate = stream$samplingRate,
    classifierIn = classifierIn,
    classifierOut = classifierOut
  )
  
}