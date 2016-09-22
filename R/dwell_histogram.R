dwell_histogram <- function(experiment, file_name = NULL) {
  if(is.character(experiment)){
    if(file.exists(paste0(experiment, "/experiment.RData")))
      load(paste0(experiment, "/experiment.RData"))
    else
      load(experiment)
  }
  
  if(is.null(file_name)){
    events <- data.frame()
    for ( i in 1:length(experiment)) {
      if(experiment[[i]]$file_data$record_type == 'test') {
        events <- rbind(events, experiment[[i]]$events)
      }
    }
    for(i in 1:length(experiment)){
      if(experiment[[i]]$file_data$record_type == 'test'){
        file_data <- experiment[[i]]$file_data
        break
      }
    }
    
    file_to_save <- paste(gsub("[[:digit:]]+.r2e", '', file_data$filename_r2e), "dwell_hist", "_", 'summary', '.jpg', sep="")
    pl_title <- 'Histogram of dwell times before click'
  } else {
    filenames <- lapply(experiment, function(x) x$file_data$filename_r2e)
    events <- experiment[[grep(file_name,filenames)]]$events
    file_data <- experiment[[grep(file_name,filenames)]]$file_data
    file_to_save <- paste(gsub("[[:digit:]]+.r2e", '', file_data$filename_r2e), "dwell_hist", "_", str_filter(file_data$filename_r2e, '([[:digit:]]+).r2e')[[1]][1], '.jpg', sep="")
    pl_title <- paste("Histogram of dwell times before click in", str_filter(file_data$filename_r2e, '([[:digit:]]+).r2e')[[1]][1])
  }
  
  events$classifier_response <- rep(NA, nrow(events))
  for (i in 1: nrow(events)){
    if (events$quick_fixation[i] == TRUE && events$activation[i] == TRUE) events$classifier_response[i] <- "true_positive"
    else if (events$quick_fixation[i] == FALSE && events$activation[i] == TRUE) events$classifier_response[i] <- "false_negative"
    else if (events$quick_fixation[i] == TRUE && events$activation[i] == FALSE) events$classifier_response[i] <- "true_negative"
  }
  events$classifier_response[which(events$false_alarm)] <- 'false_alarm'
  events$classifier_response <- factor(events$classifier_response,
                                       levels=c('false_negative','true_positive','true_negative','false_alarm'))
  
  
  p <- ggplot(data=events[which(events$classifier_response != 'false_negative'),], aes(x = dwell_time, fill = field_type)) + 
    geom_histogram(bins = (file_data$eyelines_settings$fixationDuration / file_data$eyelines_settings$delayBetweenQuickFixations) -
                     (file_data$eyelines_settings$quickFixationDuration / file_data$eyelines_settings$delayBetweenQuickFixations),
                   binwidth = file_data$eyelines_settings$delayBetweenQuickFixations, alpha = .9, colour = "#666666") + 
    labs(title = paste0(pl_title, '\n', 'N of false negative (ball) = ',
                        length(which(events$classifier_response != 'false_negative' & events$field_type == 'ball')),
                        ' (field) = ',
                        length(which(events$classifier_response != 'false_negative' & events$field_type == 'field')))) +
    labs(x="Dwell time", y="Count") +
    facet_grid(field_type ~ classifier_response) + 
    scale_fill_brewer(palette="Set2") +
    xlab("milliseconds") +
    ylab("count") +
    theme(legend.position="none")
  #     scale_fill_manual(values=c("#F37748","#067BC2"))
  
  print(p)
  ggsave(filename = file_to_save, plot = p)
  
}