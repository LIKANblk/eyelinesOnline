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
    for( i in 1:nrow(exp$events)){
      E <- exp$events[i,]
      eye <- exp$eye_data[[i]]
      
      
      if(E$false_alarm) next
      
      clf_response <- 
        if(E$quick_fixation) { 
          if(E$activation)
            'true_positive'
          else
            'true_negative'
        } else {
          if(E$activation) 'false_negative'
        }
      
      summary_list[[clf_response]][[E$field_type]] <- c(summary_list[[clf_response]][[E$field_type]], list(
        data.frame(
          t=seq(length=nrow(eye), to=end_epoch)*1000/eye_sampling_rate,
          x = eye$x,
          y= eye$y
        )
      ))
    }
  }
  
  df_for_plot <- c()
  
  for(resp in names(summary_list)) {
    
    for(event in names(summary_list[[resp]])){
      data <- summary_list[[resp]][[event]]
      
      minT <- max(sapply(data, function(X) min(X$t)))
      
      N <- abind(lapply(data, function(X){
        X[X$t >= minT,]
      }), along=3)
      
      M <- rowMeans(N,T, dims=2)
      dimnames(M) <- list(M[,'t'], c('t','x','y'))
      
      ret <- melt(M[,c('x','y')], value.name="coord")
      colnames(ret) <- c('time', 'axis', 'coord')
      ret$event = event
      ret$clf_response = resp
      
      df_for_plot <- rbind(df_for_plot, ret)
    }
  }
  df_for_plot$clf_response <- factor(df_for_plot$clf_response,
                                       levels=c('true_positive','true_negative','false_negative'))
  
  ggplot(df_for_plot, aes(x=time, y=coord)) + geom_line(aes(group=axis,colour = axis))+
    geom_vline(xintercept = 0, colour="seagreen4") +
    facet_grid(event ~ clf_response) +
    ylab("")+
    ggtitle(paste0("Eye epochs in experiment ", str_filter(experiment[[1]]$file_data$filename_edf, '.+/([[:digit:]]+)/[[:digit:]]+.edf')[[1]][2]))
  
}