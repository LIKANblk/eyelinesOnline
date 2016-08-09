create_attempts_for_different_events <- function(y, event){
  v <- vector()
  q <- 1
  for (i in 1:length(event)) {
    if(round(event[i], digits = 1) %in% round(y$time[y$event=="output"], digits = 1)){
      v[q] <- y$time[(((which(round(y$time[y$event == "output"], digits = 1) == round(event[i], digits = 1)))[1])*2)-1]
      q <- q+1
    }
  }
  v
}