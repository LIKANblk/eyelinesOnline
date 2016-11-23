draw_eeg_epochs <- function(experiment, clf_response=c('true_positive', 'true_negative', 'false_negative')) {
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
  
  summary_table <- data.frame()
  summary_eeg <- list()
  for ( i in 1:length(experiment)) {
    if(experiment[[i]]$file_data$record_type == 'test') {
      summary_table <- rbind(summary_table, experiment[[i]]$events)
      summary_eeg <- c(summary_eeg, experiment[[i]]$eeg_data$filtered_epochs)
    }
  }
  
  if(clf_response != 'false_negative'){
    df_ball_small <- melt_epochs('ball', summary_table, summary_eeg,
                                 qf, act, clf_response, end_epoch, eeg_sRate)
    df_field_small <- melt_epochs('field', summary_table, summary_eeg,
                                  qf, act, clf_response, end_epoch, eeg_sRate)
    df_for_plot_small <- rbind(df_ball_small, df_field_small)
    df_for_plot_small$type <- factor(df_for_plot_small$type, levels <- c('field', 'ball'))
  }
  
  df_ball_large <- melt_epochs('ball', summary_table, summary_eeg,
                               qf, act, clf_response, end_epoch, eeg_sRate, large_dwell = T)
  df_field_large <- melt_epochs('field', summary_table, summary_eeg,
                                qf, act, clf_response, end_epoch, eeg_sRate, large_dwell = T)
  df_for_plot_large <- rbind(df_ball_large, df_field_large)
  df_for_plot_large$type <- factor(df_for_plot_large$type, levels <- c('field', 'ball'))
  
  to_string <- as_labeller(c(`1` = 'Fz',`2` = 'F3',`3` = 'F4',`4` = 'Cz',`5` = 'C3',
                             `6` = 'C4',`7` = 'Pz', `8` = 'P1',`9` = 'P2',`10` = 'P3',`11` = 'P4',
                             `12` = 'POz',`13` = 'PO3',`14` = 'PO4',`15` = 'PO7',`16` = 'PO8',
                             `17` = 'Oz',`18` = 'O1',`19` = 'O2', `20` ='HEOG', `21` ='VEOG'))
  
  less_than_600_title <- paste0(clf_response, '. ', 'Dwell < 600', '\n',
                                sum(summary_table$quick_fixation == qf & 
                                      summary_table$activation == act &
                                      summary_table$dwell_time < 600 &
                                      summary_table$field_type == 'ball'),
                                " ball epochs and " , 
                                sum(summary_table$quick_fixation == qf &
                                      summary_table$activation == act & 
                                      summary_table$dwell_time < 600 &
                                      summary_table$field_type == 'field'),
                                " cell epochs")
  if(clf_response != 'false_negative'){ 
    more_than_600_title <- paste0(clf_response, '. ', 'Dwell >= 600', '\n',
                                  sum(summary_table$quick_fixation == qf & 
                                        summary_table$activation == act &
                                        summary_table$dwell_time >= 600 &
                                        summary_table$field_type == 'ball'),
                                  " ball epochs and " , 
                                  sum(summary_table$quick_fixation == qf &
                                        summary_table$activation == act & 
                                        summary_table$dwell_time >= 600 &
                                        summary_table$field_type == 'field'),
                                  " cell epochs")
  } else {
    more_than_600_title <- paste0(clf_response, '. ', 'Dwell == 1000', '\n',
                                  sum(summary_table$quick_fixation == qf & 
                                        summary_table$activation == act &
                                        summary_table$dwell_time >= 600 &
                                        summary_table$field_type == 'ball'),
                                  " ball epochs and " , 
                                  sum(summary_table$quick_fixation == qf &
                                        summary_table$activation == act & 
                                        summary_table$dwell_time >= 600 &
                                        summary_table$field_type == 'field'),
                                  " cell epochs")
  }
  
  if(clf_response != 'false_negative'){
    p1 <- ggplot(df_for_plot_small, aes(x=t, y=value)) +
      geom_line(aes(colour = type)) +
      ylim(-25, 25) +
      ylab("") +
      xlab("") +
      facet_wrap( ~ channel, labeller = to_string, ncol = 4) +
      geom_vline(xintercept = 0, colour="seagreen4") +
      theme(legend.position="none", plot.title = element_text(size=12)) +
      ggtitle(less_than_600_title) +
      scale_color_manual(values = c("cyan4", "firebrick2"))
  }
  if(nrow(df_for_plot_large)){
    p2 <- ggplot(df_for_plot_large, aes(x=t, y=value)) +
      geom_line(aes(colour = type)) +
      ylim(-25, 25) +
      ylab("") +
      xlab("") +
      facet_wrap( ~ channel, labeller = to_string, ncol = 4) +
      geom_vline(xintercept = 0, colour="seagreen4") +
      theme(plot.title = element_text(size=12),
            legend.title=element_blank(),
            legend.justification=c(1,0), legend.position=c(0.6,0)) +
      ggtitle(more_than_600_title) +
      scale_color_manual(labels=c("Cell", "Ball"), values = c("cyan4", "firebrick2"))
  }
  if(clf_response != 'false_negative'){
    if(exists("p2")){
      multiplot(p1, p2, cols=2)
    } else {
      pl_blank <- ggplot(mtcars, aes(x = wt, y = mpg)) + 
        xlab("") +  ylab("") +
        theme(axis.line=element_blank(),axis.text.x=element_blank(),
              axis.text.y=element_blank(),axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.title.y=element_blank(),legend.position="none")
      multiplot(p1, pl_blank, cols=2)
    }
    
  } else {
    pl_blank <- ggplot(mtcars, aes(x = wt, y = mpg)) + 
      xlab("") +  ylab("") +
      theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title.y=element_blank(),legend.position="none")
    
    multiplot(pl_blank, p2, cols=2)
  }
}

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