get_game_events <- function(file_edf){
  ans <- load.one.eye(file_edf)
  lines <- ans$events$message
  sRate <- 1000
  first_sync <- ans$sync_timestamp
  ball.choose <- sapply(str_filter(lines, ".+\"ballSelect\".+time += ([[:digit:]]+)"), function(i) (as.numeric(i[[2]]) - first_sync)/sRate)
  ball.move <- sapply(str_filter(lines, ".+\"ballMove\".+time += ([[:digit:]]+)"), function(i) (as.numeric(i[[2]]) - first_sync)/sRate)
  fixations <- sapply(str_filter(lines, "^fixation.+time += ([[:digit:]]+)"), function(i) (as.numeric(i[[2]]) - first_sync)/sRate)
  
  DF <- rbind(data.frame(type='choose', time=ball.choose), data.frame(type='move', time=ball.move), data.frame(type='fix', time=fixations))
  DF <- DF[order(DF$time, -as.numeric(DF$type)),]
  
  a <- which(DF$type=='fix')
  if(any((DF$time[a+1]-DF$time[a])>0.015)) exit('Events looks like shit')
  
  ball_choose_long_fixations <- sum(DF$type[a+1]=="choose")
  ball_move_long_fixations <- sum(DF$type[a+1]=="move")
  
  
  print(c("ball_choose_long_fixations", ball_choose_long_fixations))
  print(c("ball_move_long_fixations", ball_move_long_fixations))
  l <- list(ball.choose = ball.choose, ball.move = ball.move)
  l
}
