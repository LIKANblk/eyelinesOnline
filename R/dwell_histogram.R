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
    pl_title <- paste0('Histogram of dwell times before click in exp. ', str_filter(experiment[[1]]$file_data$filename_edf, '.+/([[:digit:]]+)/[[:digit:]]+.edf')[[1]][2])
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
  events$classifier_response[which(events$false_alarm)] <- 'false_positive'
  events$classifier_response <- factor(events$classifier_response,
                                       levels=c('false_negative','true_positive','true_negative','false_positive'))
  
  events <- events[events$classifier_response != 'false_negative',]
  events$classifier_response = factor(events$classifier_response, levels=c('true_positive','true_negative','false_positive'))
  
  to_string <- as_labeller(c(`true_positive` = 'True Positive',
                             `true_negative` = 'True Negative',
                             `false_positive` ='False Positive',
                             `ball` = 'Ball',`field` = 'Cell'))
  
  p <- ggplot(data=events[which(events$classifier_response != 'false_negative'),], aes(x = dwell_time, fill = field_type)) + 
    geom_histogram(bins = (file_data$eyelines_settings$fixationDuration / file_data$eyelines_settings$delayBetweenQuickFixations) -
                     (file_data$eyelines_settings$quickFixationDuration / file_data$eyelines_settings$delayBetweenQuickFixations),
                   binwidth = file_data$eyelines_settings$delayBetweenQuickFixations, alpha = .9, colour = "#666666") + 
    labs(title = paste0(pl_title, '\n', 'N of false negative (ball) = ',
                        length(which(events$classifier_response != 'false_negative' & events$field_type == 'ball')),
                        ' (cell) = ',
                        length(which(events$classifier_response != 'false_negative' & events$field_type == 'field')))) +
    labs(x="Dwell time", y="Count") +
    facet_grid(field_type ~ classifier_response, drop = FALSE, labeller = to_string) + 
    scale_fill_brewer(palette="Set2") +
    xlab("milliseconds") +
    ylab("count") +
    scale_x_continuous(limits = c(NA, 1000), breaks = c(300,400,500, 600, 700, 800, 900, 1000)) +
    theme(legend.position="none")
  #     scale_fill_manual(values=c("#F37748","#067BC2"))
  
  #   ggsave(filename = file_to_save, plot = p)
  p
  
}