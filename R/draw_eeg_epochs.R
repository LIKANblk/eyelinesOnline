draw_eeg_epochs <- function(experiment, clf_response) {
  
  if(is.character(experiment)){
    if(file.exists(paste0(experiment, "/experiment.RData")))
      load(paste0(experiment, "/experiment.RData"))
    else
      load(experiment)
  }
  
  if(clf_response == "true_positive") {qf = T; act = T}
  if(clf_response == "true_negative") {qf = T; act = F}
  if(clf_response == 'false_negative') {qf = F; act = T}
  
  for(exp in experiment){
    if(exp$file_data$record_type == 'test'){
      eeg_sRate <- exp$eeg_data$sampling_rate 
      end_epoch <- exp$file_data$process_settings$end_epoch
      break
    }
  }
  
  summary_table <- data.frame()
  summary_eeg <- list()
  for ( i in 1:length(experiment)) {
    if(experiment[[i]]$file_data$record_type == 'test') {
      summary_table <- rbind(summary_table, experiment[[i]]$events)
      summary_eeg <- c(summary_eeg, experiment[[i]]$eeg_data$filtered_epochs)
    }
  }
  
  
  df_ball <- melt_epochs('ball', summary_table, summary_eeg,
                         qf, act, clf_response, end_epoch, eeg_sRate)
  df_field <- melt_epochs('field', summary_table, summary_eeg,
                          qf, act, clf_response, end_epoch, eeg_sRate)
  
  
  df_for_plot <- rbind(df_ball,df_field)
  to_string <- as_labeller(c(`1` = 'Fz',`2` = 'F3',`3` = 'F4',`4` = 'Cz',`5` = 'C3',
                             `6` = 'C4',`7` = 'Pz', `8` = 'P1',`9` = 'P2',`10` = 'P3',`11` = 'P4',
                             `12` = 'POz',`13` = 'PO3',`14` = 'PO4',`15` = 'PO7',`16` = 'PO8',
                             `17` = 'Oz',`18` = 'O1',`19` = 'O2', `20` ='HEOG', `21` ='VEOG'))
  
  ggplot(df_for_plot, aes(x=t, y=value)) +
    geom_line(aes(colour = type)) +
    ylim(-25, 25) +
    ylab("") +
    xlab("") +
    facet_wrap( ~ channel, labeller = to_string, ncol = 4) +
    geom_vline(xintercept = 0, colour="seagreen4") +
    ggtitle(paste0("N of ", clf_response, " epochs = ", sum(summary_table$quick_fixation == qf &
                                                              summary_table$activation == act), '\n',
                   sum(summary_table$quick_fixation == qf & 
                         summary_table$activation == act & 
                         summary_table$field_type == 'ball'),
                   " ball epochs and " , 
                   sum(summary_table$quick_fixation == qf &
                         summary_table$activation == act & 
                         summary_table$field_type == 'field'),
                   " field epochs"))
}

melt_epochs <- function(event, summary_table, summary_eeg, qf,
                        act, clf_response, end_epoch, eeg_sRate){
  
  epochs <- summary_eeg[which(summary_table$field_type == event &
                                summary_table$quick_fixation == qf &
                                summary_table$activation == act &
                                summary_table$false_alarm == FALSE)]
  smallest_epoch <- Reduce(function(prev, x) min(prev, nrow(x)), epochs, init = Inf)
  epochs <- lapply(epochs, function(x) { x[(nrow(x)-smallest_epoch+1):nrow(x), ]})
  all_epochs <- array(unlist(epochs), dim = c(nrow(epochs[[1]]), ncol(epochs[[1]]), length(epochs)))
  mean_epochs <- apply(all_epochs, c(1,2), mean)
  mean_epochs <- mean_epochs - matrix(colMeans(mean_epochs), nrow=nrow(mean_epochs), ncol=ncol(mean_epochs), byrow = T)
  
  dimnames(mean_epochs) <- list(
    seq(length=nrow(mean_epochs), to=end_epoch, by=1000/eeg_sRate),
    1:ncol(mean_epochs)
  )
  
  df <- melt(mean_epochs, varnames = c('t','channel'))

  df$classifier_response <- clf_response
  df$type <- event
  df
}