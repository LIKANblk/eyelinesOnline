process_experiment <- function(path, channels= c(1:5, 7, 9:21), A1=22, A2=23, low = F, high = 30){
  json <- fromJSON(file = paste0(path,"meta.json"))
  experiment <- list()
  for (i in 1:length(json$'files'))
  {
    record <- list()
    file_data <- list()
    
    file_data$filename_edf <- paste0(path, json$'files'[[i]]$name_edf, '.edf')
    file_data$filename_r2e <- paste0(path, json$'files'[[i]]$name_eeg)
    
    eyetracking_data <- load.one.eye(file_data$filename_edf)
    eyetracking_messages <- eyetracking_data$events$message
    
    file_data$score <- as.numeric(str_filter(eyetracking_messages[grep('score', eyetracking_messages)], 'score\":([[:digit:]]+)')[[1]][2])
    if ( str_filter(eyetracking_messages[grep('blockButtonX', eyetracking_messages)], 'blockButtonX\":([[:digit:]]+)')[[1]][2] == "1290" ){
      file_data$button_position <- "right"
    } else {
      file_data$button_position <- "left"
    }
    file_data$record_type <- json$'files'[[i]]$'record_type'
    file_data$inverse_move_order <- json$'inverse_move_order'
    
    
    
  }
}