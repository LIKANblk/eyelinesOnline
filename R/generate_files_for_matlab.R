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
  
  eye_epochs <- list()
  for ( i in 1:length(experiment)) {
    if(experiment[[i]]$file_data$record_type == 'train') {
      eye_epochs <- c(eye_epochs, experiment[[i]]$eye_data)
    }
  }
  # was used when epochs length was always different
  # for (i in 1:length(eeg_epochs)) {
  #   attr(eeg_epochs[[i]], 'TS') <- NULL
  #   writeMat(con = paste0(folder, "/eeg_epochs/", "eeg_epoch", i, ".mat"), eeg_epoch = eeg_epochs[[i]])
  # }
  
  for(i in 1:length(eye_epochs)){
    eye_epochs[[i]] <- data.matrix(eye_epochs[[i]])
  }
  eeg_epochs <- simplify2array(eeg_epochs)
  eye_epochs <- simplify2array(eye_epochs)
  
  writeMat(con = paste0(folder, "eye_epochs.mat"), eye_epochs = eye_epochs)
  writeMat(con = paste0(folder, "eeg_epochs.mat"), eeg_epochs = eeg_epochs)
  writeMat(con = paste0(folder, "events.mat"), events = events)
}