draw_eye_epochs <- function(experiment){
  if(is.character(experiment)){
    if(file.exists(paste0(experiment, "/experiment.RData")))
      load(paste0(experiment, "/experiment.RData"))
    else
      load(experiment)
  }
  
  for(i in 1:length(experiment)){
    if(experiment[[i]]$file_data$record_type == 'test'){
      end_epoch <- experiment[[i]]$file_data$process_settings$end_epoch / 1000 * experiment[[i]]$eeg_data$sampling_rate 
      eye_sampling_rate <- experiment[[1]]$file_data$eye_sampling_rate
      break
    }
  }
  
  summary_list <- list()
  
  for(exp in experiment) {
    for( i in 1:length(exp)){
      E <- exp$events[i,]
      eye <- exp$eye_data[[i]]
      
      
      if(E$false_alarm) next
        
      clf_response= 
        if(E$quick_fixation) { 
          if(E$activation)
            'true_positive'
          else
            'true_negative'
        } else {
          if(E$activation) 'false_negative'
        }
      
      summary_list[[clf_response]] <- c(summary_list[[clf_response]], list(
        data.frame(
          t=seq(length=nrow(eye), to=end_epoch)*1000/eye_sampling_rate,
          x = eye$x,
          y= eye$y,
          field_type=E$field_type
        )
      ))
    }
  }

  for(resp in unique(summary_table$clf_response))
  {
    sel <- summary_table[summary_table$clf_response==resp,]
    
    minT <- max(sapply(sel, function(X) min(X$t)))
    
    
    
  }


  
  df_for_plot <- rbind(eye_epochs_ball_true_positive, eye_epochs_field_true_positive,
                       eye_epochs_ball_true_negative, eye_epochs_field_true_negative,
                       eye_epochs_ball_false_negative, eye_epochs_field_false_negative)
  ggplot(df_for_plot, aes(x=t)) + geom_line(aes(y=x, colour = "x")) + geom_line(aes(y=y, colour="y")) +
    ylab("")+
    geom_vline(xintercept = 0, colour="seagreen4") +
    ggtitle(paste0("Eye epochs in experiment ", str_filter(experiment[[1]]$file_data$filename_edf, '.+/([[:digit:]]+)/[[:digit:]]+.edf')[[1]][2])) +
    facet_grid(event ~ clf_response)
}

melt_eye_epochs <- function(event, summary_table, summary_xs, summary_ys, clf_response, end_epoch, eye_sampling_rate){
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
  
  df <- data.frame(x = mean_epochs_x, y = mean_epochs_y, event = event, t = seq(length=length(mean_epochs_x), to=end_epoch)*1000/eye_sampling_rate, clf_response = clf_response)
  df
}