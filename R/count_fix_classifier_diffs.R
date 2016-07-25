count_fix_classifier_diffs <- function(x){
  z <- list()
  q <- 1
  trigger <- 1
  for(i in 1:nrow(x)){
    if(x$event[i-1] == "output" && x$event[i] == "output"){
      x$event[i] <- NA
      x$time[i] <- NA
    }
  }
  x <- x[complete.cases(x),]
  for (i in 1:nrow(x)){
    if(i > 2){
      if(x$event[i-1] == "input" && x$event[i] == "output") {
        z[q] <- list(x$time[trigger:(i-1)])
        trigger <- i+1
        q <- q+1
      }
    }
  }
  z
}