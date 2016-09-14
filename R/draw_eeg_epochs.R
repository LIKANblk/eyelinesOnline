draw_eeg_epochs <- function(summary_table, summary_eeg, 
                            channels = c('Fz', 'F3', 'F4', 'Cz', 'C3', 'C4', 'Pz', 'P1', 'P2', 'P3', 'P4',
                                         'POz', 'PO3', 'PO4', 'PO7', 'PO8', 'Oz', 'O1', 'O2',
                                         'HEOG', 'VEOG')) {
  
  df_ball <- create_df_for_event('ball', summary_table, summary_eeg)
  df_field <- create_df_for_event('field', summary_table, summary_eeg)
  
  df_for_plot <- rbind(df_ball,df_field)
  colnames(df_for_plot) <- c('sample', 'channel', 'value', 'type')
  
  p <- ggplot(df_for_plot, aes(x=sample, y=value))
  p + geom_line(aes(colour = type)) + facet_wrap( ~ channel, labeller=labeller(channel = channels)) + geom_vline(xintercept = 500, colour="seagreen4")
}

create_df_for_event <- function(event, summary_table, summary_eeg) {
  epochs <- summary_eeg[which(summary_table$field_type == event)]
  smaller_epoch <- c()
  for(i in 1:length(epochs)) {
    smaller_epoch[i] <- dim(epochs[[i]])[1]
  }
  smaller_epoch <- min(smaller_epoch)
  epochs <- lapply(epochs, function(x) { x[(nrow(x)-smaller_epoch+1):nrow(x), ]})
  all_epochs <- array(unlist(epochs), dim = c(nrow(epochs[[1]]), ncol(epochs[[1]]), length(epochs)))
  mean_epochs <- apply(all_epochs, c(1,2), mean)
  mean_epochs <- mean_epochs - matrix(colMeans(mean_epochs), nrow=nrow(mean_epochs), ncol=ncol(mean_epochs), byrow = T)
  df <- melt(mean_epochs)
  df$type <- rep(event, nrow(df))
  df
}