calc_probability_estimation <- function(folder){
  path <- file.path(normalizePath(folder), "")
  json <- fromJSON(file = paste0(path,"/meta.json"))
  filename_classifier <- paste0(path, json$classifier)
  experiment <- list()
  
  resc <- 0
  inc <- 0
  
  for (i in 1:length(json$'files'))
  {
    if(json$'files'[[i]]$'record_type'!='test') next;
    
    filename_r2e <- paste0(path, json$'files'[[i]]$name_eeg)
    clf <- if(exists('classifier',json)) paste0(path,json$classifier) else filename_classifier

    classifier <- readChar(clf, file.info(clf)$size)
    classifier <- strRep(classifier, '\r', '')
    classifier <- strRep(classifier, 'RA5 <- filter_fast_events(RA4)', 'RA5 <- RA4; createOutput(ev, "IN")')
    classifier <- strRep(classifier, 'createOutput(RA4,"RES")', 'createOutput(RA4,"RES"); createOutput(ev,"IN")')
    
    data <- readStructurized(filename_r2e)
    result <- run.offline(data$streams, data$blocks, classifier)
    
    resc <- resc + length(result$RES)
    inc <- inc + length(result$IN)
    cat('.')
  }
  
  cat("\n", resc, "/", inc, " ", resc/inc, sep='')
  invisible(resc/inc)
}