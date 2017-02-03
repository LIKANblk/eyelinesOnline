allMessages <- function(data){
  ret <- data$events[data$events$type=='message', c('stTime','message')]
  names(ret) <- c("time", "message")
  ret
}