parse_quick_fixations <- function(filename, file_edf, gap_between_short_fixations = 110, long_fixation = 1000) {
  ans <- load.one.eye(file_edf)
  lines <- ans$events$message
  sRate <- 1000
  first_sync <- ans$sync_timestamp
  
  create.df <- function(lines, ev){
    evs <- str_filter(lines, paste("^", ev, ".+x += ([[:digit:]]+).+y += ([[:digit:]]+).+time += ([[:digit:]]+)", sep = ""))
    df <- data.frame(x = sapply(evs, function(x){as.numeric(x[2])}),
                     y = sapply(evs, function(x){as.numeric(x[3])}),
                     time = sapply(evs, function(x){as.numeric(x[4])}),
                     type = ev)
    df
  }
  quick.fixations <- create.df(lines, "quick fixation")
  clicks_on_long_fixations <- create.df(lines, "fixation")
  clicks_on_clf <- create.df(lines, "received")
  
  clusters <- Reduce(function(clusters, time){
    
    if(is.logical(clusters)) return( data.frame(time=time, count=1, times=I(list(time))) )
    
    last <- nrow(clusters)
    
    if( (time - clusters$time[last])<= gap_between_short_fixations ){
      clusters[nrow(clusters), ] <- list(
        time=time, 
        count=clusters$count[last]+1, 
        times=I( list( c(clusters$times[last][[1]], time) ))
      )
    } else {
      clusters[nrow(clusters)+1,] <- list(
        time=time,
        count=1,
        times=I(list(time))
      )
    }
    clusters
    
  }, quick.fixations$time, FALSE)
  
  DF <- rbind(
    data.frame(time=clusters$time, type="quick"),
    clicks_on_long_fixations[,c('time','type')], 
    clicks_on_clf[,c('time','type')])
  
  DF <- DF[order(DF$time, as.numeric(DF$type)),]
  ids <- which(DF$type=="quick")
  ids <- ids[ !DF$type[ids+1] %in% c('fixation', 'received') ]
  quick_fixes_without_clicks <- clusters[which(clusters$time %in% DF$time[ids]),]
  z <- vector(mode = "numeric")
  for (i in 1:length(quick_fixes_without_clicks$count)){
    z[i] <- ((quick_fixes_without_clicks$count[i]-1)*100)+300
  }
  
  file_to_save <- paste(gsub("[[:digit:]]+.r2e", '', filename), "quick_fixations_without_click_in_", gsub('/.+/',"",filename), '.jpg', sep="")
  file_to_save_txt <- paste(gsub("[[:digit:]]+.r2e", '', filename), "quick_fixations_without_click_in_", gsub('/.+/',"",filename), '.txt', sep="")
  
  df_for_ggplot <- data.frame(count = z, stringsAsFactors=FALSE)
  p <- ggplot(data=df_for_ggplot, aes(df_for_ggplot$count)) + 
    geom_histogram(breaks=seq(0, long_fixation - 100, by = 100), 
                   col="mediumseagreen", 
                   fill="mediumseagreen", 
                   alpha = .2) + 
    labs(title=paste("Histogram of quick fixations without click", "in", gsub('/.+/',"",filename))) +
    labs(x="fixations length", y="Count") 
  
  print(p)
  ggsave(filename = file_to_save, plot = p)
  write( paste("quick fixations without click =", z), file = file_to_save_txt)
}