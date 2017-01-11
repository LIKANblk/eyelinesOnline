generate_files_for_matlab <- function(path) {
  
  load(path)
  
  folder <-  gsub('experiment.RData', '', path)
  events <- data.frame()
  for ( i in 1:length(experiment)) {
    if(experiment[[i]]$file_data$record_type == 'train') {
      events <- rbind(events, experiment[[i]]$events)
    }
  }
  eeg_epochs <- list()
  for ( i in 1:length(experiment)) {
    if(experiment[[i]]$file_data$record_type == 'train') {
      eeg_epochs <- c(eeg_epochs, experiment[[i]]$eeg_data$filtered_epochs)
    }
  }
  # for (i in 1:length(eeg_epochs)) {
  #   attr(eeg_epochs[[i]], 'TS') <- NULL
  #   writeMat(con = paste0(folder, "/eeg_epochs/", "eeg_epoch", i, ".mat"), eeg_epoch = eeg_epochs[[i]])
  # }
  writeMat(con = paste0(folder, "events.mat"), events = events)
}