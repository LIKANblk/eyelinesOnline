process_file <- function(filename_edf, filename_r2e, file_data, filename_classifier, start_epoch,
                         end_epoch, no_eeg, gap_between_short_fixations = 100, res) {
  file_data$filename_edf <- filename_edf
  file_data$filename_r2e <- filename_r2e
  file_data$filename_classifier <- filename_classifier
  
  eyetracking_data <- load.one.eye(file_data$filename_edf)
  eyetracking_messages <- eyetracking_data$events$message
  sync_timestamp <- eyetracking_data$sync_timestamp
  
  if(!length(grep('score', eyetracking_messages)) || !length(grep('score', eyetracking_messages))) {
    stop(paste0(filename_edf, ' has no ending! Game was finished before gameOver was sent'))
  }
  
  file_data$eye_sampling_rate <- eyetracking_data$samplingRate
  
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
  gameBegin <- tail(moves$time[moves$type=='newGame'], n=1)
  gameEnd <- tail(moves$time[moves$type=='gameOver'], n=1)
  
  ## correct events$time to real fixation times
  
  long_fixations <- do.call(rbind, lapply(
    str_filter(eyetracking_messages, '^fixation in region\\.center\\.x = ([0-9.]+), region.center.y = ([0-9.]+), time = ([0-9]+)$'),
    function(str){
      data.frame(x=as.numeric(str[[2]]), y=as.numeric(str[[3]]), time= as.numeric(str[[4]]) - sync_timestamp)
    }))
  quick_fixations <- do.call(rbind, lapply(
    str_filter(eyetracking_messages, '^quick fixation in region\\.center\\.x = ([0-9.]+), region.center.y = ([0-9.]+), time = ([0-9]+)$'),
    function(str){
      data.frame(x=as.numeric(str[[2]]), y=as.numeric(str[[3]]), time= as.numeric(str[[4]]) - sync_timestamp, activation=FALSE)
    }))
  received_click <- do.call(rbind, lapply(
    str_filter(eyetracking_messages, '^received click in x = ([0-9.]+), y = ([0-9.]+), time = ([0-9]+)$'),
    function(str){
      data.frame(x=as.numeric(str[[2]]), y=as.numeric(str[[3]]), time= as.numeric(str[[4]]) - sync_timestamp, activation=TRUE)
    }))
  
  
  long_fixations <- long_fixations[ long_fixations$time>gameBegin & long_fixations$time < gameEnd, ]
  quick_fixations <- quick_fixations[ quick_fixations$time>gameBegin & quick_fixations$time < gameEnd, ]
  received_click <- received_click[ received_click$time>gameBegin & received_click$time < gameEnd, ]
  
  file_data$quick_fixation_clusters <- FALSE
  
  
  events <- 
    data.frame(
      time = long_fixations$time,
      quick_fixation = FALSE,
      activation = TRUE,
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
  
  if(length(quick_fixations)>0){
    
    df <- rbind(quick_fixations, received_click)
    df <- df[order(df$time),]
    list2cluster <- lapply( split(df, seq_along(df[,1])), as.list)
    
    list2cluster <- Reduce(function(lst, fix){
      if(is.logical(lst)) return( list(fix) )
      
      if(fix$activation){
        last <- length(lst)
        
        A <- lst[[last]]
        A$activation = TRUE
        
        c(lst[-last], list(A))
        
      } else {
        c(lst, list(fix))
      }
    }, list2cluster, FALSE)
    
    ## combine quick fixations into clusters
    clusters <- Reduce(function(clusters, fixation){
      if(is.logical(clusters)) return( data.frame(time=fixation$time, count=1, activation=fixation$activation, x=fixation$x, y=fixation$y, times=I(list(fixation$time))) )
      
      last <- clusters[nrow(clusters),]
      
      if( (fixation$time - last$time)<= gap_between_short_fixations &&
          abs( last$x - fixation$x ) <= file_data$eyelines_settings$fixationRegionSize/2 &&
          abs( last$y - fixation$y ) <= file_data$eyelines_settings$fixationRegionSize/2 &&
          last$activation == FALSE
      ){
        clusters[nrow(clusters), ] <- list(
          time=fixation$time, 
          count=last$count+1, 
          activation = fixation$activation,
          x = fixation$x,
          y = fixation$y,
          times=I( list( c(last$times[[1]], fixation$time) ))
        )
      } else {
        clusters[nrow(clusters)+1,] <- list(
          time=fixation$time,
          count=1,
          activation = fixation$activation,
          x = fixation$x,
          y = fixation$y,
          times=I(list(fixation$time))
        )
      }
      
      clusters
      
    }, list2cluster, FALSE)
    
    file_data$quick_fixation_clusters <- clusters
    
    events <- rbind(events,
                    data.frame(
                      time = clusters$time,
                      quick_fixation = TRUE,
                      activation = clusters$activation,
                      field_type = NA,
                      impossible_move = NA,
                      dwell_after_click = NA,
                      dwell_time = (clusters$count-1)*file_data$eyelines_settings$delayBetweenQuickFixations + file_data$eyelines_settings$quickFixationDuration,
                      false_alarm = NA,
                      ball_color = NA,
                      field_position = NA,
                      fixation_coords_x = clusters$x,
                      fixation_coords_y = clusters$y,
                      classifier_output = NA,
                      game_state = NA
                    )
    )
    
  }
  if(any(events$dwell_time>1000))
  {
    browser();
  }
  events <- events[order(events$time),]
  
  #remove TRUE NEGATIVES after wich long fixation presented
  ind <- which(events$quick_fixation==TRUE & events$activation == FALSE)
  ind <- ind[ind<nrow(events)]
  ind <- ind[
    events$quick_fixation[ind+1] == FALSE &
      ((events$time[ind+1] - events$time[ind]) < 2*file_data$eyelines_settings$delayBetweenQuickFixations)
    ]
  
  if(length(ind)>0){
    events <- events[-ind, ]
  }
  
  
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
        
        ind <- which(types=="BoardPositionClicked")
        if(length(ind)==0) {
          stop('Strange move sequence')
        } 
        
        ind <- ind[1]+1
        
        if(types[ind] %in% c("ballSelect", "ballDeselect"))
          return('ball')
        
        if(types[ind] %in% c('ballMove', 'blockedMove'))
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
  
  
  
  events$ball_color <- mapply(
    function(index, state){
      X <- t(file_data$game_recover$states[[state]])[index+1]
      if(is.na(X))
      {
        NA
      } else if(X==0){
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
  events <- events[-events$time<0,]
  
  eeg_data <- list()
  
  exp_name <- as.numeric(str_filter(filename_edf, '.+data/([[:digit:]]+)/')[[1]][2])
  
  eye_epochs <- list()
  
  if(!no_eeg && filename_classifier!=''){
    if(file_data$record_type != 'train'){
      eeg_data <- get_classifier_output(filename_r2e, filename_classifier, start_epoch, end_epoch, events$time, events$dwell_time)
      events$classifier_output[events$quick_fixation & events$activation] <- eeg_data$classifier_output$Q[eeg_data$classifier_output$passed] [1:sum(events$quick_fixation & events$activation)]
    } else {
      eeg_data <- get_classifier_output_train(filename_r2e, start_epoch, end_epoch, events$time, events$dwell_time, res)
    }
    
    eye_epochs <- mapply(function(current_time, current_dwell) {
      ind <- which(eyetracking_data$samples$time==current_time)
      (length(ind)==1) || browser("Can't extract eye epoch")
      
      eyetracking_data$samples[ max(1,(ind-(current_dwell-start_epoch)/1000*file_data$eye_sampling_rate)) : (ind + end_epoch/1000*file_data$eye_sampling_rate-1) , c('x','y')]
    }
    , events$time, events$dwell_time, SIMPLIFY = FALSE)
  }
  
  events$changed_selection <- FALSE
  
  if(file_data$record_type != 'train') {
    #count how many times user changed his decision about ball selection
    changed_selection <- (events$field_type[events$activation == TRUE] == 'ball')
    filtered_idx <- which(c(diff(changed_selection)==0, F) & changed_selection)
    idx <- (1:nrow(events))[events$activation == TRUE] [ filtered_idx ]
    
    rl_idx <- c()
    
    if(length(idx) > 1) {
      for(i in 1:(length(idx)-1)) {
        if(diff(events$time[idx])[i] > 100) {
          rl_idx <- c(rl_idx, idx[i])
        }
      }
    }
    
    events$changed_selection[rl_idx] <- TRUE
    events$changed_selection[which(events$changed_selection == T & events$false_alarm == T)] <- FALSE
    
    changed_selection <- length(rl_idx)
  } else {
    changed_selection <- NULL
  }
  
  file_data$changed_selection <- changed_selection
  
  list(events = events, file_data = file_data, eeg_data = eeg_data, eye_data = eye_epochs)
}