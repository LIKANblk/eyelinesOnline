process_file <- function(filename_edf, filename_r2e, file_data, filename_classifier, start_epoch, end_epoch, default_dwell, no_eeg) {
  file_data$filename_edf <- filename_edf
  file_data$filename_r2e <- filename_r2e
  
  eeg_data <- list()
  
  eyetracking_data <- load.one.eye(file_data$filename_edf)
  eyetracking_messages <- eyetracking_data$events$message
  sync_timestamp <- eyetracking_data$sync_timestamp
  
  false_alarm <- rep(FALSE, length(time))
  
  if(!length(grep('score', eyetracking_messages)) || !length(grep('score', eyetracking_messages))) {
    stop(paste0(filename_edf, ' has no ending! Game was finished before gameOver was sent'))
  }
  
  file_data$process_settings <- list(
    start_epoch = start_epoch,
    end_epoch = end_epoch,
    default_dwell = default_dwell,
    no_eeg = no_eeg
  )
  file_data$eyelines_settings <- read_all_eyelines_parameters(eyetracking_messages)
  file_data$score <- as.numeric(str_filter(eyetracking_messages[grep('score', eyetracking_messages)], 'score\":([[:digit:]]+)')[[1]][2])
  if ( file_data$eyelines_settings$blockButtonX == 1290 ){
    file_data$button_position <- "right"
  } else if( file_data$eyelines_settings$blockButtonX == 550 ) {
    file_data$button_position <- "left"
  } else {
    stop('Undefined button position!')
  }
  
  game_data <- game_state_recoverer(eyetracking_data, file_data$eyelines_settings$nCellsX, file_data$eyelines_settings$nCellsY)
  file_data$game_recover <- game_data$scheme
  file_data$events_timestamps <- game_data$events_timestamps - sync_timestamp

  time = sapply(str_filter(game_data$game_messages, 'time = ([[:digit:]]+)'), function(i) (as.numeric(i[[2]]))) - sync_timestamp
  field_type = sapply(str_filter(game_data$game_messages, 'type\":\"([[:alpha:]]+)'), function(i) (i[[2]]))
  field_type[grep('ballMove', field_type)] <- 'field'
  field_type[grep('ballSelect', field_type)] <- 'ball'
  field_type[grep('ballDeselect', field_type)] <- 'ball'
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
        if (length(which(game_data$events_timestamps - sync_timestamp == time[i]))) {
          ball_color[i] <- game_data$scheme[[which(game_data$events_timestamps - sync_timestamp == time[i])]][field_position[i]+1]
          if(ball_color[i]>100) ball_color[i] <- ball_color[i] - 100
        }
      }
    }
  }
  
  game_state <- rep(NA, length(time))
  for (i in 1:length(time)) {
    state_num <- which(time[i] == (game_data$events_timestamps - sync_timestamp))
    if(length(state_num)) {
      game_state[i] <- state_num
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
    game_state = game_state
  )
  
  toMatch = c('ballRemove','ballCreate', 'newGame')
  events <- events[time>0,]
  gameOver_time <- events$time[events$field_type == 'gameOver']
  events <- events[events$time<gameOver_time,]
  events <- events[-grep(paste(toMatch ,collapse="|"), events$field_type),]
  
  ## correct events$time to real fixation times
  
  long_fixations <- do.call(rbind, lapply(
    str_filter(eyetracking_messages, '^fixation in region\\.center\\.x = ([0-9.]+), region.center.y = ([0-9.]+), time = ([0-9]+)$'),
    function(str){
      data.frame(x=as.numeric(str[[2]]), y=as.numeric(str[[3]]), time= as.numeric(str[[4]]) - sync_timestamp)
    }))
  quick_fixations <- do.call(rbind, lapply(
    str_filter(eyetracking_messages, '^quick fixation in region\\.center\\.x = ([0-9.]+), region.center.y = ([0-9.]+), time = ([0-9]+)$'),
    function(str){
      data.frame(x=as.numeric(str[[2]]), y=as.numeric(str[[3]]), time= as.numeric(str[[4]]) - sync_timestamp)
    }))
  
  times <- sort(c(quick_fixations$time, long_fixations$time))
  
  eyetracking_event_times <- events$time
  
  events$time <- sapply(events$time, function(t){
    time <- times[ which.min(times-t <=0 )-1 ]
    t - time <= 100 || stop(sprintf("Too big variance between click event and game state change [ error time: %i %i ]", time, t))
    time
  })
  
  all_fixations <- rbind(long_fixations, quick_fixations)
  
  events$fixation_coords_x <- all_fixations$x[match(events$time, all_fixations$time)]
  events$fixation_coords_y <- all_fixations$y[match(events$time, all_fixations$time)]
  
  if(file_data$record_type == 'test') {
    
    all_quick_fixations <- eyetracking_messages[grep('quick fixation', eyetracking_messages)]
    all_quick_fixations_time <- sapply(str_filter(all_quick_fixations, 'time = ([[:digit:]]+)'), function(x) as.numeric(x[[2]]) - sync_timestamp)
    
    
    if(length(grep('report', eyetracking_messages))){
      reported_alarm <- sapply(str_filter(eyetracking_messages[grep('report', eyetracking_messages)], 'time = ([[:digit:]]+)'), function(x) as.numeric(x[[2]])) - sync_timestamp
      for ( i in 1: length(reported_alarm)) {
        events$false_alarm[sum(eyetracking_event_times<reported_alarm[i])] <- TRUE
      }
    } 
    
    true_positives <- eyetracking_messages[grep('received click', eyetracking_messages)]
    true_positives <- sapply(str_filter(true_positives, 'time = ([[:digit:]]+)'), function(x) as.numeric(x[[2]]) - sync_timestamp)
    true_positives <- true_positives[true_positives>0 & true_positives<gameOver_time]
    classifier_response <- rep(0, nrow(events))
    for ( i in 1:length(true_positives)) {
      if((eyetracking_event_times[min(which(eyetracking_event_times >= true_positives[i]))] - true_positives[i]) < 75) {
        classifier_response[min(which(eyetracking_event_times >= true_positives[i]))] <- 'true_positive'
      }
    }
    
    false_negatives <- eyetracking_messages[grep('^fixation in', eyetracking_messages)]
    false_negatives <- sapply(str_filter(false_negatives, 'time = ([[:digit:]]+)'), function(x) as.numeric(x[[2]]) - sync_timestamp)
    false_negatives <- false_negatives[false_negatives>0 & false_negatives<gameOver_time]
    for ( i in 1:length(false_negatives)) {
      if((eyetracking_event_times[min(which(eyetracking_event_times >= false_negatives[i]))] - false_negatives[i]) < 75) {
        classifier_response[min(which(eyetracking_event_times >= false_negatives[i]))] <- 'false_negative'
      }
    }
    
    events$classifier_response <- classifier_response

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
    
    indicies <- rep(NA, length(clusters$time))
    for ( i in 1:length(clusters$time)){
      if(length(which(events$time > clusters$time[i]))){
        cluster_index <- min(which(events$time > clusters$time[i]))
        if(events$time[cluster_index] - clusters$time[i] <= gap_between_short_fixations){
          indicies[i] <-cluster_index
        }
      }
    }
    
    true_negatives <- lapply(clusters$time[which(is.na(indicies))], function(x) {
      eyetracking_messages[grep(sync_timestamp + x, eyetracking_messages)]
    })
    
    length_true_negatives <- length(clusters$count[which(is.na(indicies))])
    true_negatives <- data.frame(
      time = sapply(str_filter(true_negatives, 'time = ([[:digit:]]+)'), function(x) as.numeric(x[[2]]) - sync_timestamp),
      field_type = rep(NA, length_true_negatives),
      prev_field_position = rep(NA, length_true_negatives),
      field_position = rep(NA, length_true_negatives),
      impossible_move = rep(NA, length_true_negatives),
      false_alarm = rep(FALSE, length_true_negatives),
      ball_color = rep(NA, length_true_negatives),
      game_state = rep(NA, length_true_negatives),
      fixation_coords_x = sapply(str_filter(true_negatives, 'x = ([[:digit:]]+\\.?[[:digit:]]*)'), function(x) as.numeric(x[[2]])),
      fixation_coords_y = sapply(str_filter(true_negatives, 'y = ([[:digit:]]+\\.?[[:digit:]]*)'), function(x) as.numeric(x[[2]])),
      classifier_response = rep('true_negative', length_true_negatives),
      dwell_time = (clusters$count[which(is.na(indicies))] - 1) * 100 + 300)
    
    true_negatives_in_events <- c()
    for (i in 1: nrow(true_negatives)){
      if(length( which(file_data$events_timestamps > true_negatives$time[i]))){
        true_negatives_in_events[i] <- max(which(file_data$events_timestamps < true_negatives$time[i]))
      }
    }
    
    windowWidth <- 1920
    windowHeight <- 1080
    cellSize <-  file_data$eyelines_settings$cellSize
    nCellsX <-  file_data$eyelines_settings$nCellsX
    nCellsY <-  file_data$eyelines_settings$nCellsY
    cellMargin <-  file_data$eyelines_settings$cellMargin
    true_negative_events <- c()
    for ( i in 1:length(true_negatives_in_events)){
      true_negative_events[i] <- getEventType(file_data$game_recover[[true_negatives_in_events[i]]],
                                              true_negatives$fixation_coords_x[i], true_negatives$fixation_coords_y[i],
                                              windowWidth, windowHeight, nCellsX, nCellsY, cellSize, cellMargin)
    }
    
    true_negatives$game_state[1:length(true_negatives_in_events)] <- true_negatives_in_events
    true_negatives <- true_negatives[!is.na(true_negatives$game_state),]
    true_negatives$ball_color <- true_negative_events
    
    for(i in 1:nrow(true_negatives)){
      if(true_negatives$ball_color[i] == 0){
        true_negatives$field_type[i] = 'field'
      } else if (true_negatives$ball_color[i] > 0){
        true_negatives$field_type[i] = 'ball'
      }
    }
    
    events$dwell_time <- rep(1000, nrow(events))
    events$dwell_time[indicies[!is.na(indicies)]] <- (clusters$count[which(!is.na(indicies))]-1)*100+300
    events <- rbind(events, true_negatives)
    events <- events[order(events$time),]
    if(!no_eeg){
      eeg_data <- get_classifier_output(filename_r2e, filename_classifier, start_epoch, end_epoch, events$time, events$dwell_time)
    }
    
  } else {
    events$dwell_time <- rep(default_dwell, nrow(events))
    true_negatives = c()
  }
  
  
  get_eye_epochs <- function(current_time, current_dwell, xy) {
    res <- mapply(function(current_time, current_dwell) {
      if(which(eyetracking_data$samples$time == current_time) > abs(start_epoch)){
        eyetracking_data$samples[[xy]][
          (which(eyetracking_data$samples$time == current_time)+start_epoch) : (which(eyetracking_data$samples$time == current_time) + end_epoch + current_dwell)]
      }
    }
    , current_time, current_dwell)
    t(res)
  }
  eye_epochs_x <- get_eye_epochs( events$time, events$dwell_time, 'x')
  eye_epochs_y <- get_eye_epochs( events$time, events$dwell_time, 'y')
  
  list(events = events, file_data = file_data, eeg_data = eeg_data, eye_epochs_x = eye_epochs_x, eye_epochs_y = eye_epochs_y,  true_negatives = true_negatives)
}