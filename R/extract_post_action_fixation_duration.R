extract_post_action_fixation_duration <- function(record){
  data <- read.edf(record$file_data$file_edf)
  pulses <- getSynchroPulses(data)
  timeLimit <- checkSynchroPulses(pulses)
  
  events <- record$events
  
  times <- timeLimit$begin + events$time
  
  eyeVarX <- if(data$begin$eyes == 2) 'gxR' else 'gxL'
  eyeVarY <- if(data$begin$eyes == 2) 'gyR' else 'gyL'
  
  regionSize <- read_all_eyelines_parameters(data)$fixationBlockRegionSize/2
  
  mapply(function(time, x, y){
    first <- which(data$samples$time> time)[[1]]
    
     min(
       which(abs(x - data$samples[first + 1:10000, eyeVarX]) > regionSize)[[1]], 
       which(abs(y - data$samples[first + 1:10000, eyeVarY]) > regionSize)[[1]]
       ) * data$begin$sampleRate/1000
      
  }, times, events$fixation_coords_x, events$fixation_coords_y)
  
}