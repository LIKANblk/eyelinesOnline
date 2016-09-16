draw_eeg_epochs <- function(summary_table, summary_eeg, clf_response) {
  
  smaller_epoch <- c()
  for(i in 1:length(summary_eeg)) {
    smaller_epoch[i] <- dim(summary_eeg[[i]])[1]
  }
  smaller_epoch <- min(smaller_epoch)
  
  df_ball <- create_df_for_event('ball', summary_table, summary_eeg, smaller_epoch)
  df_field <- create_df_for_event('field', summary_table, summary_eeg, smaller_epoch)
  
  df_for_plot <- rbind(df_ball,df_field)
  colnames(df_for_plot) <- c('sample', 'channel', 'value', 'classifier_response', 'type')
  to_string <- as_labeller(c(`1` = 'Fz',`2` = 'F3',`3` = 'F4',`4` = 'Cz',`5` = 'C3',
                             `6` = 'C4',`7` = 'Pz', `8` = 'P1',`9` = 'P2',`10` = 'P3',`11` = 'P4',
                             `12` = 'POz',`13` = 'PO3',`14` = 'PO4',`15` = 'PO7',`16` = 'PO8',
                             `17` = 'Oz',`18` = 'O1',`19` = 'O2', `20` ='HEOG', `21` ='VEOG'))
  
  p <- ggplot(df_for_plot[df_for_plot$classifier_response == clf_response, ], aes(x=sample, y=value))
  p + geom_line(aes(colour = type)) +
    facet_wrap( ~ channel, labeller = to_string) +
    geom_vline(xintercept = 500, colour="seagreen4") +
    ggtitle(paste0("N of ", clf_response, " epochs = ", sum(summary_table$classifier_response == clf_response &
                                                              summary_table$field_type != 'blockedMove'), '\n',
                   sum(summary_table$classifier_response == clf_response & summary_table$field_type == 'ball'),
                   " ball epochs and " , 
                   sum(summary_table$classifier_response == clf_response & summary_table$field_type == 'field'),
                   " field epochs"))
}

create_df_for_event <- function(event, summary_table, summary_eeg, smaller_epoch) {
  epochs_true_positive <- melt_epochs(event, summary_table, summary_eeg, 'true_positive', smaller_epoch)
  epochs_false_negative <- melt_epochs(event, summary_table, summary_eeg, 'false_negative',smaller_epoch)
  epochs_true_negative <- melt_epochs(event, summary_table, summary_eeg, 'true_negative', smaller_epoch)
  df <- rbind(epochs_true_positive, epochs_false_negative, epochs_true_negative)
  df
}

melt_epochs <- function(event, summary_table, summary_eeg, classifier_response, smaller_epoch){
  
  epochs <- summary_eeg[which(summary_table$field_type == event &
                                summary_table$classifier_response == classifier_response &
                                summary_table$false_alarm == FALSE &
                                summary_table$field_type != 'blockedMove')]
  epochs <- lapply(epochs, function(x) { x[(nrow(x)-smaller_epoch+1):nrow(x), ]})
  all_epochs <- array(unlist(epochs), dim = c(nrow(epochs[[1]]), ncol(epochs[[1]]), length(epochs)))
  mean_epochs <- apply(all_epochs, c(1,2), mean) - matrix(colMeans(mean_epochs), nrow=nrow(mean_epochs), ncol=ncol(mean_epochs), byrow = T)
  df <- melt(mean_epochs)
  df$classifier_response <- classifier_response
  df$type <- event
  df
}