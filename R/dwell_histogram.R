dwell_histogram <- function(events, file_data) {
  button_dwells_file <- paste(gsub("[[:digit:]]+.r2e", '', file_data$filename_r2e), "button_dwell_hist", "_", str_filter(file_data$filename_r2e, '([[:digit:]]+).r2e')[[1]][1], '.jpg', sep="")
  field_dwells_file <- paste(gsub("[[:digit:]]+.r2e", '', file_data$filename_r2e), "field_dwell_hist", "_", str_filter(file_data$filename_r2e, '([[:digit:]]+).r2e')[[1]][1], '.jpg', sep="")
  
  p <- ggplot(data=events, aes(x = dwell_time, fill = field_type)) + 
    geom_histogram(bins = (file_data$long_fixation_duration / file_data$delay_between_quick_fixations) -
                     (file_data$quick_fixation_duration / file_data$delay_between_quick_fixations),
                   alpha = .9, colour = "#333333") + 
    labs(title=paste("Histogram of dwell times before click in",
                     str_filter(file_data$filename_r2e, '([[:digit:]]+).r2e')[[1]][1])) +
    labs(x="Dwell time", y="Count") +
    facet_grid(classifier_response ~ field_type) + 
    # scale_fill_brewer(palette="Set2") 
    scale_fill_manual(values=c("#F37748","#067BC2"))
  
  print(p)
  ggsave(filename = file_to_save, plot = p)
  
  #     pp <- qplot()
  #     pp <- pp + 
  #       labs(title=paste("No events ", plot_name ," were found in" , gsub('/.+/',"",filename), "!")) + 
  #       theme_bw() +
  #       theme(plot.title = element_text(colour = "red"))
  #     print(pp)
  #     ggsave(filename = file_to_save, plot = pp)
  
  
}