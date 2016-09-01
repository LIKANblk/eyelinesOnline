process_file <- function(filename_edf, filename_r2e, file_data, filename_classifier, start_epoch, end_epoch, default_dwell, no_eeg) {
  record <- list()
  
  file_data$filename_edf <- filename_edf
  file_data$filename_r2e <- filename_r2e
  
  eeg_data <- list()
  
  eyetracking_data <- load.one.eye(file_data$filename_edf)
  eyetracking_messages <- eyetracking_data$events$message
  
  false_alarm <- rep(FALSE, length(time))
  
  file_data$score <- as.numeric(str_filter(eyetracking_messages[grep('score', eyetracking_messages)], 'score\":([[:digit:]]+)')[[1]][2])
  if ( str_filter(eyetracking_messages[grep('blockButtonX', eyetracking_messages)], 'blockButtonX\":([[:digit:]]+)')[[1]][2] == "1290" ){
    file_data$button_position <- "right"
  } else {
    file_data$button_position <- "left"
  }
  
  file_data$quick_fixation_duration <- as.numeric(str_filter(eyetracking_messages[grep('quickFixationDuration', eyetracking_messages)], 'quickFixationDuration\":([[:digit:]]+)')[[1]][2])
  file_data$delay_between_quick_fixations <- as.numeric(str_filter(eyetracking_messages[grep('delayBetweenQuickFixations', eyetracking_messages)], 'delayBetweenQuickFixations\":([[:digit:]]+)')[[1]][2])
  file_data$long_fixation_duration <- as.numeric(str_filter(eyetracking_messages[grep('fixationDuration', eyetracking_messages)], 'fixationDuration\":([[:digit:]]+)')[[1]][2])
  
  
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
  
  game_state <- rep(NA, length(time))
  for (i in 1:length(time)) {
    state_num <- which(time[i] == (game_data$events_timestamps - eyetracking_data$sync_timestamp))
    if(length(state_num)) {
      game_state[i] <- state_num
    }
  }
  
  all_quick_fixations <- eyetracking_messages[grep('quick fixation', eyetracking_messages)]
  all_quick_fixations_time <- sapply(str_filter(all_quick_fixations, 'time = ([[:digit:]]+)'), function(x) as.numeric(x[[2]]) - eyetracking_data$sync_timestamp)
  fixation_coords_x <- rep(0, length(time))
  fixation_coords_y <- rep(0, length(time))
  for ( i in 1: length(time)) {
    if(time[i]> 0){
      fixation_coords_x[i] <- as.numeric(str_filter(
        all_quick_fixations[sum(all_quick_fixations_time<time[i])],
        'x = ([[:digit:]]+\\.?[[:digit:]]*)')[[1]][2]) 
      fixation_coords_y[i] <- as.numeric(str_filter(
        all_quick_fixations[sum(all_quick_fixations_time<time[i])],
        'y = ([[:digit:]]+\\.?[[:digit:]]*)')[[1]][2]) 
    }
  }
  
  events <- data.frame(
    time = time,
    field_type = field_type,
    prev_field_position = prev_field_position,
    field_position = field_position,
    impossible_move = impossible_move,
    false_alarm = false_alarm,
    ball_color = ball_color,
    game_state = game_state,
    fixation_coords_x = fixation_coords_x,
    fixation_coords_y = fixation_coords_y
  )
  
  toMatch = c('ballRemove','ballCreate', 'newGame')
  events <- events[time>0,]
  gameOver_time <- events$time[events$field_type == 'gameOver']
  events <- events[events$time<gameOver_time,]
  events <- events[-grep(paste(toMatch ,collapse="|"), events$field_type),]
  
  if(file_data$record_type == 'test') {
    
    if(length(grep('report', eyetracking_messages))){
      reported_alarm <- sapply(str_filter(eyetracking_messages[grep('report', eyetracking_messages)], 'time = ([[:digit:]]+)'), function(x) as.numeric(x[[2]])) - eyetracking_data$sync_timestamp
      for ( i in 1: length(reported_alarm)) {
        false_alarm[sum(time<reported_alarm[i])] <- TRUE
      }
    } 
    
    true_positives <- eyetracking_messages[grep('received click', eyetracking_messages)]
    true_positives <- sapply(str_filter(true_positives, 'time = ([[:digit:]]+)'), function(x) as.numeric(x[[2]]) - eyetracking_data$sync_timestamp)
    true_positives <- true_positives[true_positives>0 & true_positives<gameOver_time]
    classifier_response <- rep(0, nrow(events))
    for ( i in 1:length(true_positives)) {
      if((events$time[min(which(events$time >= true_positives[i]))] - true_positives[i]) < 75) {
        classifier_response[min(which(events$time >= true_positives[i]))] <- 'true_positive'
      }
    }
    
    false_negatives <- eyetracking_messages[grep('^fixation in', eyetracking_messages)]
    false_negatives <- sapply(str_filter(false_negatives, 'time = ([[:digit:]]+)'), function(x) as.numeric(x[[2]]) - eyetracking_data$sync_timestamp)
    false_negatives <- false_negatives[false_negatives>0 & false_negatives<gameOver_time]
    for ( i in 1:length(false_negatives)) {
      if((events$time[min(which(events$time >= false_negatives[i]))] - false_negatives[i]) < 75) {
        classifier_response[min(which(events$time >= false_negatives[i]))] <- 'false_negative'
      }
    }
    
    events$classifier_response <- classifier_response
    
    dwell_times <- data.frame(time = events$time, type = events$classifier_response)
    dwell_times <- rbind(
      dwell_times,
      data.frame(time = all_quick_fixations_time, type = rep('quick', length(all_quick_fixations_time))))
    dwell_times <- dwell_times[order(dwell_times$time),]
    dwell_times <- dwell_times[dwell_times$time > 0,]
    
    
    all_quick_fixations_time <- all_quick_fixations_time[all_quick_fixations_time>0]
    
    gap_between_short_fixations <- 130
    
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
      
    }, all_quick_fixations_time, FALSE)
    
    cluster_for_event <- sapply(events$time, function(time){
      idx <- which(clusters$time>=time)
      if(length(idx)==0) stop('Can\'t find cluster for event')
      idx <- idx[[1]]
      
      if(max(clusters$times[[idx]] - time) >= -75){
        idx
      } else {
        if(idx<2) stop('Can\'t find cluster for event')
        if(max(clusters$times[[idx-1]] - time) >= -75){
          idx-1
        } else {
          stop('Can\'t find cluster for event')
        }
      }
    })
    
    events$dwell_time <- clusters$count[cluster_for_event]*100+300
    if(!no_eeg){
      eeg_data <- get_classifier_output(filename_r2e, filename_classifier, start_epoch, end_epoch, events$time, events$dwell_time)
    }
  } else {
    events$dwell_time <- rep(default_dwell, nrow(events))
  }
  
  get_eye_epochs <- function(current_time, current_dwell, xy) {
    res <- mapply(function(current_time, current_dwell) {
      eyetracking_data$samples[[xy]][
        (which(eyetracking_data$samples$time == current_time)+start_epoch) : (which(eyetracking_data$samples$time == current_time) + end_epoch + current_dwell)]
    }, current_time, current_dwell)
    t(res)
  }
  eye_epochs_x <- get_eye_epochs( events$time, events$dwell_time, 'x')
  eye_epochs_y <- get_eye_epochs( events$time, events$dwell_time, 'y')
  
  save(events = events, file_data = file_data, eeg_data = eeg_data, eye_epochs_x = eye_epochs_x,  eye_epochs_y = eye_epochs_y, file = gsub("r2e", "RData", filename_r2e))
  list(events = events, file_data = file_data, eeg_data = eeg_data, eye_epochs_x = eye_epochs_x, eye_epochs_y = eye_epochs_y)
}