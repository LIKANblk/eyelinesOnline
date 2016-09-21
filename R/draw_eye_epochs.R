draw_eye_epochs <- function(experiment){
  
  for(i in 1:length(experiment)){
    if(experiment[[i]]$file_data$record_type == 'test'){
      end_epoch <- experiment[[i]]$file_data$process_settings$end_epoch / 1000 * experiment[[i]]$eeg_data$sampling_rate 
      eye_sampling_rate <- experiment[[1]]$file_data$
      break
    }
  }
  
  if(is.character(experiment)){
    experiment <- 
      if(file.exists(paste0(experiment, "/experiment.RData")))
        load(paste0(experiment, "/experiment.RData"))$experiment
    else
      load(experiment)$experiment
  }
  
  summary_table <- data.frame()
  summary_xs <- list()
  summary_ys <- list()
  for ( i in 1:length(experiment)) {
    if(experiment[[i]]$file_data$record_type == 'test') {
      summary_table <- rbind(summary_table, experiment[[i]]$events)
      summary_xs <- c(summary_xs, experiment[[i]]$eye_epochs_x)
      summary_ys <- c(summary_ys, experiment[[i]]$eye_epochs_y)
    }
  }
  
  eye_epochs_ball_true_positive <- melt_eye_epochs('ball', summary_table, summary_xs, summary_ys, 'true_positive', end_epoch)
  eye_epochs_field_true_positive <- melt_eye_epochs('field', summary_table, summary_xs, summary_ys, 'true_positive', end_epoch)
  eye_epochs_ball_true_negative <- melt_eye_epochs('ball', summary_table, summary_xs, summary_ys, 'true_negative', end_epoch)
  eye_epochs_field_true_negative <- melt_eye_epochs('field', summary_table, summary_xs, summary_ys, 'true_negative', end_epoch)
  eye_epochs_ball_false_negative <- melt_eye_epochs('ball', summary_table, summary_xs, summary_ys, 'false_negative', end_epoch)
  eye_epochs_field_false_negative <- melt_eye_epochs('field', summary_table, summary_xs, summary_ys, 'false_negative', end_epoch)
  
  df_for_plot <- rbind(eye_epochs_ball_true_positive, eye_epochs_field_true_positive,
                       eye_epochs_ball_true_negative, eye_epochs_field_true_negative,
                       eye_epochs_ball_false_negative, eye_epochs_field_false_negative)
  ggplot(df_for_plot, aes(x=t)) + geom_line(aes(y=x, colour = "x")) + geom_line(aes(y=y, colour="y")) +
    ylab("")+
    geom_vline(xintercept = 0, colour="seagreen4") +
    ggtitle(paste0("Eye epochs in experiment ", str_filter(experiment[[1]]$file_data$filename_edf, '.+/([[:digit:]]+)/[[:digit:]]+.edf')[[1]][2])) +
    facet_grid(event ~ clf_response)
}

melt_eye_epochs <- function(event, summary_table, summary_xs, summary_ys, clf_response, end_epoch){
  if(clf_response == "true_positive") {qf = T; act = T}
  if(clf_response == "true_negative") {qf = T; act = F}
  if(clf_response == 'false_negative') {qf = F; act = T}
  epochs_x <- summary_xs[which(summary_table$field_type == event &
                                 summary_table$quick_fixation == qf &
                                 summary_table$activation == act &
                                 summary_table$false_alarm == FALSE)]
  epochs_y <-  summary_ys[which(summary_table$field_type == event &
                                  summary_table$quick_fixation == qf &
                                  summary_table$activation == act &
                                  summary_table$false_alarm == FALSE)]
  smallest_epoch <- Reduce(function(prev, x) min(prev, length(x)), epochs_x, init = Inf)
  epochs_x <- lapply(epochs_x, function(x) { x[(length(x)-smallest_epoch+1):length(x) ]})
  epochs_y <- lapply(epochs_y, function(x) { x[(length(x)-smallest_epoch+1):length(x) ]})
  
  mean_epochs_x <- colMeans(do.call(rbind, epochs_x), na.rm = T)
  mean_epochs_y <- colMeans(do.call(rbind, epochs_y), na.rm = T)
  
  df <- data.frame(x = mean_epochs_x, y = mean_epochs_y, event = event, t = seq(length=length(mean_epochs_x), to=end_epoch), clf_response = clf_response)
  df
}