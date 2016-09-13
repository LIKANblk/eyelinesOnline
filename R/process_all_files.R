process_experiment <- function(path, start_epoch = -500, end_epoch = 1500, channels= c(1:5, 7, 9:21), A1=22, A2=23, low = F, high = 30,
                               filename_classifier, default_dwell = 500, no_eeg = F){
  json <- fromJSON(file = paste0(path,"meta.json"))
  filename_classifier <- paste0(path, json$classifier)
  experiment <- list()
  for (i in 1:length(json$'files'))
  {
    file_data <- list()
    filename_edf <- paste0(path, json$'files'[[i]]$name_edf)
    filename_r2e <- paste0(path, json$'files'[[i]]$name_eeg)
    
    file_data$record_type <- json$'files'[[i]]$'record_type'
    file_data$inverse_move_order <- json$'inverse_move_order'
    
    record <- process_file(filename_edf, filename_r2e, file_data, filename_classifier, start_epoch, end_epoch, default_dwell, no_eeg)
    experiment[[i]] <- record 
    print(json$'files'[[i]]$name_eeg)
  }
  summary_table <- data.frame()
  summary_eeg <- list()
  for ( i in 1:length(experiment)) {
    if(experiment[[i]]$file_data$record_type == 'test') {
      summary_table <- rbind(summary_table, experiment[[i]]$events)
      summary_eeg <- c(summary_eeg, experiment[[i]]$eeg_data$filtered_epochs)
    }
  }
  draw_eeg_epochs(summary_table, summary_eeg)
  dwell_histogram(summary_table, experiment[[i]]$file_data, summary_table = T)
  
  experiment
}