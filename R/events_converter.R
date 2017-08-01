EventsConverter <- function(source_path,target_path){
  # Converts data from .edf and .r2e files to matlab readable formats (.mat and .csv)
  # This functions for each pair of .edf and .r2e files writes to the target dir three files:
  # eeg_data_<digit>.mat - file contains eeg data (matrix times x channels) and sampling rate. Time counts from recieving synchro pulses in 33 channel
  #  commonly eeg record longer than eyetracking records because function doesn't controll endig synchro pulse
  # eyetracking_data_<digit>.mat -file containt's sample rate and struct with filds: time, coordinates (x,y) and pupil size. Time counted from synchro pulse 
  
  # events_<digit>.csv - file in CSV format (semicolon as separator) consist of three colomns - event's time (in ms) calculating from synchropulse, 
  # event's type (message, saccade or fixation start and end etc) and text of message for 'message' events types
  # Args:
  #   source_path: Path to folder  with arbitrary number of .edf and .r2e files. They have to be listed in meta.json file in the same dir
  #   target_path: Folder where resulting files will be placed
  #      with no missing values.
  #
  # Returns:
  #   Nothing
  library('R.matlab')
  path <- file.path(normalizePath(source_path), "")
  json <- fromJSON(file = paste0(source_path,"/meta.json"))
  experiment <- list()
  for (i in 1:length(json$'files'))
  {
    file_data <- list()
    filename_edf <- paste0(path, json$'files'[[i]]$name_edf)
    filename_r2e <- paste0(path, json$'files'[[i]]$name_eeg)
    
    file_data$record_type <- json$'files'[[i]]$'record_type'
    file_data$date <- json$'date'
    
    if(substr(file_data$record_type,0,2)=='! ') next;
    
    file_data$inverse_move_order <- json$'inverse_move_order'
    
    
    ExtractEyeData(filename_edf,target_path,i)
    ExtractEEG(filename_r2e, target_path,i)
  
    experiment <- c(experiment, list(record))
    print(json$'files'[[i]]$name_eeg)
  }
}

ExtractEEG <- function(filenameR2e, targetPath,recordNumber){
  data <- R3::extractAllChannels(filenameR2e)
  sync_ind <- which(diff(bitwAnd(data$EEG[,33],2))==2)[3]
  writeMat( con = paste0(targetPath, sprintf("eeg_data_%d.mat",recordNumber)),
          eegData =  data$EEG[sync_ind:nrow(data$EEG),],samplingRate=attr(data$EEG,'.StreamInfo')$samplingRate)
}

ExtractEyeData <- function(filenameEdf,targetPath,recordNumber){
  eyetrackingData <- load.one.eye(filenameEdf)
  eyetrackingMessages <- eyetrackingData$events$message
  if(!length(grep('score', eyetrackingMessages)) || !length(grep('score', eyetrackingMessages))) {
    stop(paste0(filenameEdf, ' has no ending! Game was finished before gameOver was sent'))
  }
  
  eyetrackingData$samples$time = 2*eyetrackingData$samples$time
  writeMat( con = paste0(targetPath, sprintf("eyetracking_data_%d.mat",recordNumber)), 
            eyetrackingData = eyetrackingData$samples ,samplingRate = eyetrackingData$samplingRate)
  
  events <-data.frame(time=eyetrackingData$events$stTime,type=sapply(eyetrackingData$events$type,as.character),
                      messages=sapply(eyetrackingData$events$message,function(x) gsub('[\n\t]','',x)),
                      stringsAsFactors=FALSE)
  events <- events[events$time >= 0,]
  events$time = 2*events$time
  write.table(events, file = paste0(targetPath, sprintf("events_%d.csv",recordNumber)),row.names=FALSE, na="",col.names=TRUE, sep=";")
}

#ProcessSourceFiles <- function(filename_edf, filename_r2e,record_number, target_path){
#  library('R.matlab')
#  ExtractEyeData(filename_edf,target_path,record_number)
#  ExtractEEG(filename_r2e, target_path,record_number)
#}