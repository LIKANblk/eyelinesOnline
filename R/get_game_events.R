get_game_events <- function(file_edf, filename){
  
  file_to_save <- paste(gsub("[[:digit:]]+.r2e", '', filename), "long_fix_game_events_in_", gsub('/.+/',"",filename), '.txt', sep="")
  
  ans <- load.one.eye(file_edf)
  lines <- ans$events$message
  sRate <- 1000
  first_sync <- ans$sync_timestamp
  ball.choose <- sapply(str_filter(lines, ".+\"ballSelect\".+time += ([[:digit:]]+)"), function(i) (as.numeric(i[[2]]) - first_sync)/sRate)
  ball.move <- sapply(str_filter(lines, ".+\"ballMove\".+time += ([[:digit:]]+)"), function(i) (as.numeric(i[[2]]) - first_sync)/sRate)
  fixations <- sapply(str_filter(lines, "^fixation.+time += ([[:digit:]]+)"), function(i) (as.numeric(i[[2]]) - first_sync)/sRate)
  
  DF <- rbind(data.frame(type='choose', time=ball.choose), data.frame(type='move', time=ball.move), data.frame(type='fix', time=fixations))
  DF <- DF[order(DF$time, -as.numeric(DF$type)),]
  for (i in 1:nrow(DF)){
    if(i < nrow(DF)){
      if(DF$type[i] == "fix" && DF$type[i+1] == "fix"){
        DF$type[i] = NA
        DF$time[i] = NA
      }
    } else {
      if(DF$type[i] == "fix"){
        DF$type[i] = NA
        DF$time[i] = NA
      }
    }
  }
  
  DF <- DF[(complete.cases(DF)),]
  
  a <- which(DF$type=='fix')
  if(any((DF$time[a+1]-DF$time[a])>0.020)) stop('Events looks like shit')
  
  ball_choose_long_fixations <- sum(DF$type[a+1]=="choose")
  ball_move_long_fixations <- sum(DF$type[a+1]=="move")
  
  
  write(c( paste("ball_choose_long_fixations =", ball_choose_long_fixations),
           paste("ball_move_long_fixations =", ball_move_long_fixations)),
        file = file_to_save)
  l <- list(ball.choose = ball.choose, ball.move = ball.move)
  l
}
