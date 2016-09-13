dwell_histogram <- function(events, file_data, summary_table = F) {
  
  if(summary_table){
    file_to_save <- paste(gsub("[[:digit:]]+.r2e", '', file_data$filename_r2e), "dwell_hist", "_", 'summary', '.jpg', sep="")
    pl_title <- 'Histogram of dwell times before click'
  } else {
    file_to_save <- paste(gsub("[[:digit:]]+.r2e", '', file_data$filename_r2e), "dwell_hist", "_", str_filter(file_data$filename_r2e, '([[:digit:]]+).r2e')[[1]][1], '.jpg', sep="")
    pl_title <- paste("Histogram of dwell times before click in", str_filter(file_data$filename_r2e, '([[:digit:]]+).r2e')[[1]][1])
  }
  
  p <- ggplot(data=events[which(events$false_alarm != TRUE),], aes(x = dwell_time, fill = field_type)) + 
    geom_histogram(bins = (file_data$long_fixation_duration / file_data$delay_between_quick_fixations) -
                     (file_data$quick_fixation_duration / file_data$delay_between_quick_fixations),
                   binwidth = file_data$delay_between_quick_fixations, alpha = .9, colour = "#666666") + 
    labs(title = pl_title) +
    labs(x="Dwell time", y="Count") +
    facet_grid(classifier_response ~ field_type) + 
    scale_fill_brewer(palette="Set2") 
  #     scale_fill_manual(values=c("#F37748","#067BC2"))
  
  if(summary_table){
    print(p)
  } 
  ggsave(filename = file_to_save, plot = p)
  
}