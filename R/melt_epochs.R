melt_epochs <- function(event, summary_table, summary_eeg, qf,
                        act, clf_response, end_epoch, eeg_sRate, large_dwell = F){
  
  indicies <- which(summary_table$field_type == event &
                      summary_table$quick_fixation == qf &
                      summary_table$activation == act &
                      summary_table$false_alarm == FALSE)
  
  if(large_dwell) {
    indicies <- indicies[which(indicies %in% which(summary_table$dwell_time >= 600))]
  } else {
    indicies <- indicies[which(indicies %in% which(summary_table$dwell_time < 600))]
  }
  
  if(length(indicies) == 0){
    df <- data.frame(t = NA, channel = NA, value = NA, classifier_response = NA, type = NA)[numeric(0), ]
    return(df)
    break()
  }
  
  epochs <- summary_eeg[indicies]
  
  smallest_epoch <- Reduce(function(prev, x) min(prev, nrow(x)), epochs, init = Inf)
  epochs <- lapply(epochs, function(x) { x[(nrow(x)-smallest_epoch+1):nrow(x), ]})
  all_epochs <- array(unlist(epochs), dim = c(nrow(epochs[[1]]), ncol(epochs[[1]]), length(epochs)))
  mean_epochs <- apply(all_epochs, c(1,2), mean, na.rm=TRUE)
  mean_epochs <- mean_epochs - matrix(colMeans(mean_epochs, na.rm=TRUE), nrow=nrow(mean_epochs), ncol=ncol(mean_epochs), byrow = T)
  
  dimnames(mean_epochs) <- list(
    seq(length=nrow(mean_epochs), to=end_epoch, by=1000/eeg_sRate),
    1:ncol(mean_epochs)
  )
  
  df <- melt(mean_epochs, varnames = c('t','channel'))
  
  df$classifier_response <- clf_response
  df$type <- event
  df
}