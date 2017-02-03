getSynchroPulses <- function(data){
  messages <- allMessages(data)
  regexp <- "^!CMD \\d+ write_ioport 0x8 (\\d+)$"
  messages <- messages[str_detect(messages$message, regexp),]
  sent <- str_match_all(messages$message, regexp)
  data.frame(time=messages$time, sent=as.numeric(simplify2array(sent)[,2,]))
}