process_file <- function(filename_edf, filename_r2e, file_data, filename_classifier, start_epoch, end_epoch, no_eeg,   gap_between_short_fixations = 130) {
  file_data$filename_edf <- filename_edf
  file_data$filename_r2e <- filename_r2e
  file_data$filename_classifier <- filename_classifier
  
  eyetracking_data <- load.one.eye(file_data$filename_edf)
  eyetracking_messages <- eyetracking_data$events$message
  sync_timestamp <- eyetracking_data$sync_timestamp
  
  if(!length(grep('score', eyetracking_messages)) || !length(grep('score', eyetracking_messages))) {
    stop(paste0(filename_edf, ' has no ending! Game was finished before gameOver was sent'))
  }
  
  file_data$process_settings <- list(
    start_epoch = start_epoch,
    end_epoch = end_epoch,
    no_eeg = no_eeg,
    gap_between_short_fixations = gap_between_short_fixations
  )
  
  file_data$eyelines_settings <- read_all_eyelines_parameters(eyetracking_messages)
  file_data$score <- as.numeric(str_filter(eyetracking_messages, 'score\":([[:digit:]]+)')[[1]][2])
  
  if ( file_data$eyelines_settings$blockButtonX == 1290 ){
    file_data$button_position <- "right"
  } else if( file_data$eyelines_settings$blockButtonX == 550 ) {
    file_data$button_position <- "left"
  } else {
    stop('Undefined button position!')
  }
  
  
  game_recover <- game_state_recoverer(eyetracking_data, file_data$eyelines_settings$nCellsX, file_data$eyelines_settings$nCellsY)
  moves <- game_recover$moves
  
  file_data$game_recover <- list(states= game_recover$states, times=game_recover$times)
  file_data$move_durations = game_recover$move_durations
  
  ## game begin and end
  gameBegin <- moves$time[moves$type=='newGame']
  gameEnd <- moves$time[moves$type=='gameOver']
  
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
  received_click <- do.call(rbind, lapply(
    str_filter(eyetracking_messages, '^received click in x = ([0-9.]+), y = ([0-9.]+), time = ([0-9]+)$'),
    function(str){
      data.frame(x=as.numeric(str[[2]]), y=as.numeric(str[[3]]), time= as.numeric(str[[4]]) - sync_timestamp)
    }))
  
  
  long_fixations <- long_fixations[ long_fixations$time>gameBegin & long_fixations$time < gameEnd, ]
  quick_fixations <- quick_fixations[ quick_fixations$time>gameBegin & quick_fixations$time < gameEnd, ]
  received_click <- received_click[ received_click$time>gameBegin & received_click$time < gameEnd, ]
  
  ## combine quick fixations into clusters
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
    
  }, quick_fixations$time, FALSE)
  
  file_data$quick_fixation_clusters <- clusters
  
  events <- 
    data.frame(
      time = long_fixations$time,
      quick_fixation = FALSE,
      activation = NA,
      field_type = NA,
      impossible_move = NA,
      dwell_after_click = NA,
      dwell_time = file_data$eyelines_settings$fixationDuration,
      false_alarm = NA,
      ball_color = NA,
      field_position = NA,
      fixation_coords_x = long_fixations$x,
      fixation_coords_y = long_fixations$y,
      classifier_output = NA,
      game_state = NA
    )
  
  if(!is.logical(clusters)){
    cluster2quick = match(clusters$time, quick_fixations$time)
    
    events <- rbind(events,
      data.frame(
        time = clusters$time,
        quick_fixation = TRUE,
        activation = NA,
        field_type = NA,
        impossible_move = NA,
        dwell_after_click = NA,
        dwell_time = clusters$count*file_data$eyelines_settings$delayBetweenQuickFixations + file_data$eyelines_settings$quickFixationDuration,
        false_alarm = NA,
        ball_color = NA,
        field_position = NA,
        fixation_coords_x = quick_fixations$x[cluster2quick],
        fixation_coords_y = quick_fixations$y[cluster2quick],
        classifier_output = NA,
        game_state = NA
      )
    )
  }
  events <- events[order(events$time),]
  
  
  time_pairs <- cbind( events$time, c(events$time[-1], Inf))
  
  events$field_type <- apply(time_pairs, 1, function(X){
    types <- moves$type[moves$time>=X[1] & moves$time<X[2]]
    if(length(types)>=1){
      
      if(types[1] %in% c("ClickedToUnlock","ClickedToLock")) 
        return('button')
      
      if(types[1] == 'BallClickedInBlockedMode')
        return('ball')
      if(types[1] == 'BoardClickedInBlockedMode')
        return('field')
      
      if(length(types)>=2){
        
        (types[1]=="BoardPositionClicked") || stop('Strange move sequence')
        
        if(types[2] %in% c("ballSelect", "ballDeselect")) 
          return('ball')
        
        if(types[2] %in% c('ballMove', 'blockedMove')) 
          return('field')
      }
      
      if(types[1] == 'BoardPositionClicked')
        return('field')
    }
    return('')
  })
  
  events$impossible_move <- apply(time_pairs, 1 ,function(X){
    types <- moves$type[moves$time>=X[1] & moves$time<X[2]]
    
    sum(types == 'blockedMove')>0
  })
  
  reported_alarm <- as.numeric(sapply(str_filter(eyetracking_messages ,'user reported false activation, time = ([[:digit:]]+)'), function(x) x[[2]]) ) - sync_timestamp
  
  events$false_alarm <- apply(time_pairs, 1 ,function(X){
    sum(reported_alarm >= X[1] & reported_alarm < X[2])>0
  })
  
  # events$ball_color[events$field_type=='ball'] <- apply(time_pairs[events$field_type=='ball',], 1, function(X){
  #   mv <- moves[moves$time>=X[1] & moves$time<X[2],]
  #   mv <- mv[mv$type=='ballSelect', ]
  #   t(
  #     file_data$game_recover$states[[
  #       tail(
  #         which(file_data$game_recover$times<=mv$time),
  #         n=1
  #         )
  #       ]]
  #     )[mv$index+1]-100  # 100 is constant that marks selected ball
  # })
  
  events$field_position <- mapply(function(X,Y) get_field_index(X,Y, file_data$eyelines_settings), events$fixation_coords_x, events$fixation_coords_y, SIMPLIFY = TRUE);
  
  events$game_state <- apply(time_pairs, 1, function(X){
    W <- which(file_data$game_recover$times>= X[1] & file_data$game_recover$times < X[2])
    if(length(W)>0){
      W[[1]]
    } else {
      tail(which(file_data$game_recover$times <=X[1]), n=1)
    }
  })
  
  
  events$dwell_after_click  <- mapply(function(time, x, y){
    first <- which(eyetracking_data$samples$time> time)[[1]]
    
    (min(
      which(abs(x - eyetracking_data$samples$x[first + 1:10000]) > file_data$eyelines_settings$fixationBlockRegionSize/2)[[1]], 
      which(abs(y - eyetracking_data$samples$y[first + 1:10000]) > file_data$eyelines_settings$fixationBlockRegionSize/2)[[1]]
    ) - 1) * 1000/eyetracking_data$samplingRate 
    
  }, events$time, events$fixation_coords_x, events$fixation_coords_y)
  
  
  events$activation <- events$field_type!=''
  
  events$ball_color <- mapply(
    function(index, state){
      X <- t(file_data$game_recover$states[[state]])[index+1]
      if(X==0){
        NA 
      } else {
        if(X>100) X-100 else X
      }
    }, 
    events$field_position,
    events$game_state
  )
  
  unknownField <- events$field_type==''
  events$field_type[unknownField] <- c('ball', 'field')[is.na(events$ball_color[unknownField])+1]
  
  eeg_data <- list()
  
  if(!no_eeg && filename_classifier!='' && file_data$record_type=='test'){
    eeg_data <- get_classifier_output(filename_r2e, filename_classifier, start_epoch, end_epoch, events$time, events$dwell_time)
    
    events$classifier_output[events$quick_fixation & events$activation] <- eeg_data$classifier_output$Q[eeg_data$classifier_output$passed] [1:sum(events$quick_fixation & events$activation)]
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
  
  list(events = events, file_data = file_data, eeg_data = eeg_data, eye_epochs_x = eye_epochs_x, eye_epochs_y = eye_epochs_y)
}