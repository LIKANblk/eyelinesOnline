create_attempts_for_different_events <- function(y, event){
  v <- vector()
  q <- 1
  for (i in 1:length(event)) {
    if(ceil(event[i]) %in% ceil(y$time[y$event=="output"])){
      v[q] <- y$time[((which(ceil(y$time[y$event == "output"]) == ceil(event[i])))*2)-1]
      q <- q+1
    }
  }
  v
}