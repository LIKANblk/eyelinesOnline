process_experiment <- function(path, channels= c(1:5, 7, 9:21), A1=22, A2=23, low = F, high = 30){
  json <- fromJSON(file = paste0(path,"meta.json"))
  experiment <- list()
  for (i in 1:length(json$'files'))
  {
    file_data <- list()
    filename_edf <- paste0(path, json$'files'[[i]]$name_edf)
    filename_r2e <- paste0(path, json$'files'[[i]]$name_eeg)
    
    file_data$record_type <- json$'files'[[i]]$'record_type'
    file_data$inverse_move_order <- json$'inverse_move_order'
    
    record <- process_file(filename_edf, filename_r2e, file_data)
    experiment[[i]] <- record 
  }
  experiment
}