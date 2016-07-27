draw_attempts_plot <- function(z,long_fixation,gap_between_short_fixations, plot_name, filename) {
  file_to_save <- paste(gsub("[[:digit:]]+.r2e", '', filename), plot_name, "-", gsub('/.+/',"",filename), '.jpg', sep="")
  if(length(z)>0){
    diff_fix_clf <- sapply(z, function(x) { ceiling(diff(unlist(x))*1000)})
    diff_fix_clf <- diff_fix_clf[lapply(diff_fix_clf,length)>0]
    attempts <- vector(mode = 'numeric', length=length(diff_fix_clf))
    for (i in 1:length(diff_fix_clf)){
      for(ii in 1:min(length(diff_fix_clf[[i]]), long_fixation/gap_between_short_fixations)){
        if(diff_fix_clf[[i]][length(diff_fix_clf[[i]])-ii+1] <= gap_between_short_fixations+70){
          attempts[i] <- attempts[i]+1
        } else {
          break
        }
      }
    }
    
    attempts <- attempts+1
    df <- data.frame(attempts = attempts, stringsAsFactors=FALSE)
    p <- ggplot(data=df, aes(df$attempts)) + 
      geom_histogram(breaks=seq(0, (long_fixation/gap_between_short_fixations), by = 1), 
                     col="salmon", 
                     fill="salmon", 
                     alpha = .2) + 
      labs(title=paste("Histogram of attempts to run classifier when", plot_name, "in", gsub('/.+/',"",filename))) +
      labs(x="N of attempts", y="Count") 
    
    print(p)
    ggsave(filename = file_to_save, plot = p)
  } else {
    pp <- qplot()
    pp <- pp + 
      labs(title=paste("No events ", plot_name ," were found in" , gsub('/.+/',"",filename), "!")) + 
      theme_bw() +
      theme(plot.title = element_text(colour = "red"))
    print(pp)
    ggsave(filename = file_to_save, plot = pp)
  }
  
}