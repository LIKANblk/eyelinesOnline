draw_eeg_epochs <- function(experiment, clf_response) {
  
  if(is.character(experiment)){
    experiment <- 
      if(file.exists(paste0(experiment, "/experiment.RData")))
        load(paste0(experiment, "/experiment.RData"))$experiment
      else
        load(experiment)$experiment
  }
  
  summary_table <- data.frame()
  summary_eeg <- list()
  for ( i in 1:length(experiment)) {
    if(experiment[[i]]$file_data$record_type == 'test') {
      summary_table <- rbind(summary_table, experiment[[i]]$events)
      summary_eeg <- c(summary_eeg, experiment[[i]]$eeg_data$filtered_epochs)
    }
  }
  
  
  df_ball <- melt_epochs('ball', summary_table, summary_eeg, clf_response)
  df_field <- melt_epochs('field', summary_table, summary_eeg, clf_response)
  
  
  
  
  
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

melt_epochs <- function(event, summary_table, summary_eeg, classifier_response){
  
  epochs <- summary_eeg[which(summary_table$field_type == event &
                                summary_table$classifier_response == classifier_response &
                                summary_table$false_alarm == FALSE &
                                summary_table$field_type != 'blockedMove')]
  smaller_epoch <- Reduce(function(prev, x) min(prev, nrow(x)), epochs)
  
  epochs <- sapply(epochs, function(x) { x[(nrow(x)-smaller_epoch+1):nrow(x), ]})
  all_epochs <- array(unlist(epochs), dim = c(nrow(epochs[[1]]), ncol(epochs[[1]]), length(epochs)))
  mean_epochs <- apply(all_epochs, c(1,2), mean) - matrix(colMeans(mean_epochs), nrow=nrow(mean_epochs), ncol=ncol(mean_epochs), byrow = T)
  df <- melt(mean_epochs)
  df$classifier_response <- classifier_response
  df$type <- event
  df
}