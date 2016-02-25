offline_classifier <- function(res, file_r2e, file_edf) {
  require(Resonance)
  require(eyelinesOnline)
  require(hybridEyeEEG)
  
  
  signal <- R3::extractChannel(file_r2e,0)
  sync_marks <- which( diff(signal[,ncol(signal)])>0 )
  signal <- signal[(sync_marks[3]+1):nrow(signal), ]
  
  actions <- extract.actions(file_edf)
  
  msgbuttonPressed_t <- ceiling(actions[which(actions$Type=="msgbuttonPressed"),1]*1E6)
  msgballChosen_t <- ceiling(actions[which(actions$Type=="msgballChosen"),1]*1E6)
  msgBallMoved_t <- ceiling(actions[which(actions$Type=="msgBallMoved"),1]*1E6)
  eventsT_t = c(msgbuttonPressed_t, msgballChosen_t, msgBallMoved_t)
  msgev <- rep("GFY", length(eventsT_t))
  
  input1 <- source.channels(signal, samplingRate=500)
  input2 <- source.events(msgev, eventsT_t)
  
  input <- function(x){
    if(x==1){ input1 }else{ input2 }
  }
  
  RM <- diag(nrow=33)[res$channels,]
  FS <- pipeline(
    input(1),
    signalPreparation(, low=res$low, high=res$high, notch=50),
    pipe.spatial(, RM),
    pipe.references(, c(res$A1,res$A2))
  )
  
  RA1 <- pipe.decimate(FS, 1, 20 , coef_10000_to_500)
  ev <- input(2)
  RA2 <- cross.windowizeByEvents(RA1, ev, res$t/20, shift=-res$t/20)
  RA3 <- pipe.medianWindow(RA2, 1, 12)
  RA4 <- pipe.trof.classifier(RA3, res$W, res$th, res$ufeats )
  
  number_of_clicks <- sum(sapply(RA4, function(x) is.null(x)))
  print(c("Number of recognized clicks = ", number_of_clicks))
  
  RA4
  
}