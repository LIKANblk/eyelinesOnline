EventsConverter <- function(source_path,target_path){
  path <- file.path(normalizePath(source_path), "")
  json <- fromJSON(file = paste0(source_path,"/meta.json"))
  experiment <- list()
  for (i in 1:length(json$'files'))
  {
    file_data <- list()
    filename_edf <- paste0(source_path, json$'files'[[i]]$name_edf)
    filename_r2e <- paste0(source_path, json$'files'[[i]]$name_eeg)
    
    file_data$record_type <- json$'files'[[i]]$'record_type'
    file_data$date <- json$'date'
    
    if(substr(file_data$record_type,0,2)=='! ') next;
    
    file_data$inverse_move_order <- json$'inverse_move_order'
    
    record <- process_file(filename_edf, filename_r2e, file_data, clf, start_epoch, end_epoch, no_eeg, res = res)
    experiment <- c(experiment, list(record))
    print(json$'files'[[i]]$name_eeg)
  }
}

ExctractEEG <- function(filename_r2e,events){
  data <- R3::extractAllChannels(filename_r2e)$streamName
  syncTS <- attr(data, 'TS')[ which(diff(bitwAnd(data[,33],2))==2)[3]  ]
  time_events <- source.events(rep('', length(times)), times*1E3+syncTS)
}
ProcessSourceFiles <- function(filename_edf, filename_r2e, file_data){
  file_data$filename_edf <- filename_edf
  file_data$filename_r2e <- filename_r2e
  eyetracking_data <- load.one.eye(file_data$filename_edf)
  
  eyetracking_messages <- eyetracking_data$events$message
  sync_timestamp <- eyetracking_data$sync_timestamp
  
  if(!length(grep('score', eyetracking_messages)) || !length(grep('score', eyetracking_messages))) {
    stop(paste0(filename_edf, ' has no ending! Game was finished before gameOver was sent'))
  }
  
}