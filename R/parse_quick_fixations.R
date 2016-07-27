parse_quick_fixations <- function(filename, file_edf, gap_between_short_fixations = 100, long_fixation = 2000) {
  ans <- load.one.eye(file_edf)
  lines <- ans$events$message
  sRate <- 1000
  first_sync <- ans$sync_timestamp
  quick.fixations <- sapply(str_filter(lines, "^quick fixation.+time += ([[:digit:]]+)"), function(i) (as.numeric(i[[2]]) - first_sync))  
  clicks_on_long_fixations <- sapply(str_filter(lines, "^fixation.+time += ([[:digit:]]+)"), function(i) (as.numeric(i[[2]]) - first_sync))
  clicks_on_clf <- sapply(str_filter(lines, "^received click.+time += ([[:digit:]]+)"), function(i) (as.numeric(i[[2]]) - first_sync))
  DF <- rbind(
    data.frame(time = quick.fixations, type = "quick_fixation"),
    data.frame(time = clicks_on_long_fixations, type = "click_on_long_fixation"),
    data.frame(time = clicks_on_clf, type = "click_on_clf")
  )
  DF <- DF[order(DF$time, as.numeric(DF$type)),]
  DF$type = sapply(DF$type, as.character)
  max_n_quick_fix <-(long_fixation-500)/ gap_between_short_fixations
  for (i in 1:nrow(DF)){
    if(DF$type[i] == "click_on_clf" || DF$type[i] == "click_on_long_fixation"){
      for(ii in 1:max_n_quick_fix){
        if(ii < i){
          if(DF$time[i] - DF$time[i-(min(ii, (i-1)))] <= long_fixation){
            DF$type[i-ii] = "remove"
          }
        } else {
          break
        }
      }
      DF$type[i] = "remove"
    }
  }
  
  quickFixes <- DF$time[which(DF$type!="remove")]
  qf <- str_filter(lines, "^quick fixation.+x += ([[:digit:]]+).+y += ([[:digit:]]+).+time += ([[:digit:]]+)")
  qfsdf <- data.frame(x = sapply(qf, function(x) as.numeric(x[2])), y = sapply(qf, function(x) as.numeric(x[3])), time = sapply(qf, function(x) as.numeric(x[4])- first_sync))
  lookOnlyQuickFixes <- qfsdf[which(qfsdf$time %in% quickFixes), ]
  lookOnlyQuickFixes <- cbind(lookOnlyQuickFixes, data.frame(attempt="", stringsAsFactors = F))
  q <- 1
  for ( i in 1:nrow(lookOnlyQuickFixes)){
    if(i == 1 ){
      lookOnlyQuickFixes$attempt[i] <- as.character(q)
    } else {
      
    }
  }
}