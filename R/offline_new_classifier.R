offline_new_classifier <- function(res, file_r2e, file_edf) {
  require(Resonance)
  require(eyelinesOnline)
  require(hybridEyeEEG)
  
  
  signal <- R3::extractChannel(file_r2e,0)
  sync_marks <- which( diff(signal[,ncol(signal)])>0 )
#  signal <- signal[(sync_marks[3]+1):nrow(signal), ]
  
  actions <- extract.actions(file_edf)
  
  msgbuttonPressed_t <- ceiling(actions[which(actions$Type=="msgbuttonPressed"),1]*1E6)
  msgballChosen_t <- ceiling(actions[which(actions$Type=="msgballChosen"),1]*1E6)
  msgBallMoved_t <- ceiling(actions[which(actions$Type=="msgBallMoved"),1]*1E6)
  msgBallClickedInBlockedMode_t <- ceiling(actions[which(actions$Type=="msgBallClickedInBlockedMode"),1]*1E6)
  
  eventsT_t = c(msgbuttonPressed_t)# msgballChosen_t, msgBallMoved_t)
  msgev <- rep("GFY", length(eventsT_t))
  
  input1 <- source.channels(signal, samplingRate=500)
  input2 <- source.events(msgev, eventsT_t)
  
  input <- function(x){
    if(x==1){ input1 }else{ input2 }
  }
  
  FS <- signalPreparation(input(1), low=res$low, high=res$high, notch=50, refs=c(res$A1, res$A2), channels=res$channels)
  
#  FS <- FS[(sync_marks[3]+1):nrow(signal), ]
#  FS <- source.channels(FS, samplingRate=500)
  
  online_epoch_start = min(res$bsln_start, res$times_seq[1])
  online_epoch_end = max(res$bsln_end, (res$times_seq + res$decimation_window)) 
   
  ev <- input(2)
  RA2 <- cross.windowizeByEvents(FS, ev, online_epoch_end/1000*SI(FS)$samplingRate, shift=online_epoch_start/1000*SI(FS)$samplingRate)
  RA3 <- pipe.medianWindow(RA2, (res$bsln_start)/1000* SI(RA2)$samplingRate, (res$bsln_end)/1000* SI(RA2)$samplingRate)
  RA4 <- pipe.trof.classifier2(RA3, res$W, res$th, res$times_seq/1000, 0.05)
  
  number_of_clicks <- sum(sapply(RA4, function(x) is.null(x)))
  print(c("Number of recognized clicks = ", number_of_clicks))
  
  RA4
  
}