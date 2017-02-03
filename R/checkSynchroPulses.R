checkSynchroPulses <- function(pulses){
  if(!identical(pulses$sent, c(0, 2, 0, 2, 0, 2, 0, 15, 0, 2, 0, 2, 0, 2, 0, 15))) warning("Incorrect sequence of pulses")
  
  #   if(any(abs(diff(pulses$time[1:8]) - c(50, 150, 50, 150, 50, 150, 50))>=2)) warning("Timing errors in initial sequence")
  #   if(any(abs(diff(pulses$time[9:16]) - c(50, 150, 50, 150, 50, 150, 50))>=2)) warning("Timing errors in finalization sequence")
  
  list(begin=pulses$time[6], end = pulses$time[16])
}