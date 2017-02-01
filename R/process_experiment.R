process_experiment <- function(path, start_epoch = -1000, end_epoch = 1000, channels= c(1:5, 7, 9:21), A1=22, A2=23, low = F, high = 30,
                               filename_classifier, no_eeg = F){
  
  path <- file.path(normalizePath(path), "")
  json <- fromJSON(file = paste0(path,"/meta.json"))
  filename_classifier <- paste0(path, json$classifier)
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
    
    clf <- if(exists('classifier',json)) paste0(path,json$classifier) else filename_classifier
    res <- list(
      low = low,
      high = high,
      A1 = A1,
      A2 = A2,
      channels = channels
    )
    record <- process_file(filename_edf, filename_r2e, file_data, clf, start_epoch, end_epoch, no_eeg, res = res)
    experiment <- c(experiment, list(record))
    print(json$'files'[[i]]$name_eeg)
  }
  save(experiment = experiment, file = paste0(path, "/experiment.RData"))
  cat('Processing experiment №', str_filter(path, '.+/([[:digit:]]+)/$')[[1]][2], 'done!\n')
  invisible(experiment)
}