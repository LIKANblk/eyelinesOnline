load.one.eye <- function(fileName){
  data <- read.edf(fileName)
  if(is.null(data)) stop("Can't read EDF file")
  
  #check for errors
  if(any(data$samples$errors!=0)) warning("There is errors in samples")
  if(any(data$events$status!=0)) warning("There is errors in events$status")
  #if(any(data$events$flags!=0)) warning("There is errors in events$flags")
  
  if(data$begin$eyes==1){
    # left eye
    eyeData <- data.frame(
      time=data$samples$time,
      x = data$samples$gxL,
      y = data$samples$gyL,
      pupil = data$samples$paL)
  }else if(data$begin$eyes==2){
    #right eye
    eyeData <- data.frame(
      time=data$samples$time,
      x = data$samples$gxR,
      y = data$samples$gyR,
      pupil = data$samples$paR)
  }else{
    stop("There are two eyes recorded in file, waiting only one")
  }
  pulses <- getSynchroPulses(data)
  timeLimit <- checkSynchroPulses(pulses);
  
  # drop data
  eyeData <- eyeData[
    (eyeData$time>=timeLimit$begin),
    ]
  eyeData$time <- eyeData$time - timeLimit$begin
  # fix events
  events <- data$events
  events$stTime <- events$stTime - timeLimit$begin
  events$enTime <- events$enTime - timeLimit$begin
  
  fixation.duration <- as.numeric((str_filter(events$message, 'fixationDuration.+:([[:digit:]]+)'))[[1]][[2]])
  
  SRstartSacc <- data$events[data$events$type=='startsacc' , 'stTime'] - timeLimit$begin
  SRendSacc <- data$events[data$events$type=='endsacc' , 'stTime'] - timeLimit$begin
  SRstartFix <- data$events[data$events$type=='startfix' , 'stTime'] - timeLimit$begin
  SRendFix <- data$events[data$events$type=='endfix' , 'stTime'] - timeLimit$begin
  
  list(
    samples = eyeData,
    events = events,
    samplingRate = data$begin$sampleRate,
    experimentDur = timeLimit$end - timeLimit$begin,
    sync_timestamp = timeLimit$begin,
    pulses = pulses,
    fixation.duration = fixation.duration,
    SRstartSacc = SRstartSacc[SRstartSacc>0],
    SRendSacc = SRendSacc[SRendSacc>0],
    SRstartFix = SRstartFix[SRstartFix>0],
    SRendFix = SRendFix[SRendFix>0]
  )
}