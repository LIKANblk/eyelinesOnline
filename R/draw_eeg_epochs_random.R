draw_eeg_epochs_random <- function(experiment, clf_response=c('true_positive', 'true_negative', 'false_negative')) {
  clf_response <- match.arg(clf_response)
  
  
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
  
  #create data frame of normal classifier epochs
  summary_table_normal <- data.frame()
  summary_eeg_normal <- list()
  for ( i in 1:length(experiment)) {
    if(experiment[[i]]$file_data$record_type == 'test') {
      summary_table_normal <- rbind(summary_table_normal, experiment[[i]]$events)
      summary_eeg_normal <- c(summary_eeg_normal, experiment[[i]]$eeg_data$filtered_epochs)
    }
  }
  
  #create data frame of random classifier epochs
  summary_table_random <- data.frame()
  summary_eeg_random <- list()
  for ( i in 1:length(experiment)) {
    if(experiment[[i]]$file_data$record_type == 'random') {
      summary_table_random <- rbind(summary_table_random, experiment[[i]]$events)
      summary_eeg_random <- c(summary_eeg_random, experiment[[i]]$eeg_data$filtered_epochs)
    }
  }
  
  
  df_ball_normal <- melt_epochs('ball', summary_table_normal, summary_eeg_normal,
                                qf, act, clf_response, end_epoch, eeg_sRate)
  df_field_normal <- melt_epochs('field', summary_table_normal, summary_eeg_normal,
                                 qf, act, clf_response, end_epoch, eeg_sRate)
  df_for_plot_normal <- rbind(df_ball_normal, df_field_normal)
  df_for_plot_normal$type <- factor(df_for_plot_normal$type, levels <- c('field', 'ball'))
  
  
  df_ball_random <- melt_epochs('ball', summary_table_random, summary_eeg_random,
                                qf, act, clf_response, end_epoch, eeg_sRate)
  df_field_random <- melt_epochs('field', summary_table_random, summary_eeg_random,
                                 qf, act, clf_response, end_epoch, eeg_sRate)
  df_for_plot_random <- rbind(df_ball_random, df_field_random)
  df_for_plot_random$type <- factor(df_for_plot_random$type, levels <- c('field', 'ball'))
  
  to_string <- as_labeller(c(`1` = 'Fz',`2` = 'F3',`3` = 'F4',`4` = 'Cz',`5` = 'C3',
                             `6` = 'C4',`7` = 'Pz', `8` = 'P1',`9` = 'P2',`10` = 'P3',`11` = 'P4',
                             `12` = 'POz',`13` = 'PO3',`14` = 'PO4',`15` = 'PO7',`16` = 'PO8',
                             `17` = 'Oz',`18` = 'O1',`19` = 'O2', `20` ='HEOG', `21` ='VEOG'))
  
  normal_clf_title <- paste0('Normal classifier, ', clf_response, '. ', 'Dwell == 500', '\n',
                             sum(summary_table_normal$quick_fixation == qf & 
                                   summary_table_normal$activation == act &
                                   summary_table_normal$dwell_time == 500 &
                                   summary_table_normal$changed_selection == FALSE &
                                   summary_table_normal$false_alarm == FALSE &
                                   summary_table_normal$field_type == 'ball'),
                             " ball epochs and " , 
                             sum(summary_table_normal$quick_fixation == qf &
                                   summary_table_normal$activation == act & 
                                   summary_table_normal$dwell_time == 500 &
                                   summary_table_normal$changed_selection == FALSE &
                                   summary_table_normal$false_alarm == FALSE &
                                   summary_table_normal$field_type == 'field'),
                             " cell epochs")
  
  random_clf_title <- paste0('Random classifier, ', clf_response, '. ', 'Dwell == 500', '\n',
                             sum(summary_table_random$quick_fixation == qf & 
                                   summary_table_random$activation == act &
                                   summary_table_random$dwell_time == 500 &
                                   summary_table_random$changed_selection == FALSE &
                                   summary_table_random$false_alarm == FALSE &
                                   summary_table_random$field_type == 'ball'),
                             " ball epochs and " , 
                             sum(summary_table_random$quick_fixation == qf &
                                   summary_table_random$activation == act & 
                                   summary_table_random$dwell_time == 500 &
                                   summary_table_random$changed_selection == FALSE &
                                   summary_table_random$false_alarm == FALSE &
                                   summary_table_random$field_type == 'field'),
                             " cell epochs")
  
  
  if(nrow(df_for_plot_normal)>1 & nrow(df_for_plot_random)>1){
    p1 <- ggplot(df_for_plot_normal, aes(x=t, y=value)) +
      geom_line(aes(colour = type)) +
      ylim(-25, 25) +
      ylab("") +
      xlab("") +
      facet_wrap( ~ channel, labeller = to_string, ncol = 4) +
      geom_vline(xintercept = 0, colour="seagreen4") +
      theme(legend.position="none", plot.title = element_text(size=12)) +
      ggtitle(normal_clf_title) +
      scale_color_manual(labels=c("Cell", "Ball"), values = c("cyan4", "firebrick2"))
    
    p2 <- ggplot(df_for_plot_random, aes(x=t, y=value)) +
      geom_line(aes(colour = type)) +
      ylim(-25, 25) +
      ylab("") +
      xlab("") +
      facet_wrap( ~ channel, labeller = to_string, ncol = 4) +
      geom_vline(xintercept = 0, colour="seagreen4") +
      theme(plot.title = element_text(size=12),
            legend.title=element_blank(),
            legend.justification=c(1,0), legend.position=c(0.6,0)) +
      ggtitle(random_clf_title) +
      scale_color_manual(labels=c("Cell", "Ball"), values = c("cyan4", "firebrick2"))
    
    
    multiplot(p1, p2, cols=2)
  }
  
}