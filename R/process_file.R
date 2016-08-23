process_file <- function(filename_edf, filename_r2e, file_data) {
  record <- list()
  
  file_data$filename_edf <- filename_edf
  file_data$filename_r2e <- filename_r2e
  
  eyetracking_data <- load.one.eye(file_data$filename_edf)
  eyetracking_messages <- eyetracking_data$events$message
  
  file_data$score <- as.numeric(str_filter(eyetracking_messages[grep('score', eyetracking_messages)], 'score\":([[:digit:]]+)')[[1]][2])
  if ( str_filter(eyetracking_messages[grep('blockButtonX', eyetracking_messages)], 'blockButtonX\":([[:digit:]]+)')[[1]][2] == "1290" ){
    file_data$button_position <- "right"
  } else {
    file_data$button_position <- "left"
  }
  
  
  game_data <- game_state_recoverer(eyetracking_data)
  file_data$game_recover <- game_data$scheme
  file_data$events_timestamps <- game_data$events_timestamps - eyetracking_data$sync_timestamp
  record$file_data <- file_data
  
  time = sapply(str_filter(game_data$game_messages, 'time = ([[:digit:]]+)'), function(i) (as.numeric(i[[2]]))) - eyetracking_data$sync_timestamp
  field_type = sapply(str_filter(game_data$game_messages, 'type\":\"([[:alpha:]]+)'), function(i) (i[[2]]))
  field_type[grep('ballMove', field_type)] <- 'field'
  field_type[grep('ballSelect', field_type)] <- 'ball'
  field_type[grep('ClickedToUnlock', field_type)] <- 'button'
  
  prev_field_position <- vector(mode = "numeric", length = length(time))
  prev_field_position[grep('field', field_type)] = sapply(str_filter(game_data$game_messages, '\"from\":([[:digit:]]+)'), function(i) (as.numeric(i[[2]])))
  
  field_position <- rep(NA, length(time))
  field_position[grep('field', field_type)] = sapply(str_filter(game_data$game_messages, '\"to\":([[:digit:]]+)'), function(i) (as.numeric(i[[2]])))
  field_position[grep('index', game_data$game_messages)] = sapply(str_filter(game_data$game_messages, '\"index\":([[:digit:]]+)'), function(i) (as.numeric(i[[2]])))
  
  impossible_move <- rep(FALSE, length(time))
  impossible_move[grep('blockedMove', field_type)] <- TRUE
  
  false_alarm <- rep(FALSE, length(time))
  if(file_data$record_type == 'test'){
    reported_alarm <- sapply(str_filter(eyetracking_messages[grep('report', eyetracking_messages)], 'time = ([[:digit:]]+)'), function(x) as.numeric(x[[2]])) - eyetracking_data$sync_timestamp
    for ( i in 1: length(reported_alarm)) {
      false_alarm[sum(time<reported_alarm[i])] <- TRUE
    }
  } 
  
  ball_color <- rep(0, length(time))
  for (i in 1:length(time)){
    if(time[i]>0){
      if(is.na(field_position[i])){
        ball_color[i] <- 0
      } else {
        if (length(which(game_data$events_timestamps - eyetracking_data$sync_timestamp == time[i]))) {
          ball_color[i] <- game_data$scheme[[which(game_data$events_timestamps - eyetracking_data$sync_timestamp == time[i])]][field_position[i]+1]
          if(ball_color[i]>100) ball_color[i] <- ball_color[i] - 100
        }
      }
    }
  }
  
  events <- data.frame(
    time = time,
    field_type = field_type,
    prev_field_position = prev_field_position,
    field_position = field_position,
    impossible_move = impossible_move,
    false_alarm = false_alarm,
    ball_color = ball_color
  )
  
  
  list(events = events, file_data = file_data)
  
}