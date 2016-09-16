game_state_recoverer <- function(eyetracking_data)
{
  lines <- eyetracking_data$events$message
  first_sync <- eyetracking_data$sync_timestamp
  game_messages <- lines[grep('gm', lines)]
  toMatch <- c('BoardPositionClicked', 'BoardClickedInBlockedMode', 'BallClickedInBlockedMode',
               'BoardClickedInBlockedMode', 'random_block_starts', 'random_block_ends', 
               'ReachedMaximumMovesQuantity')
  game_messages <- game_messages[-grep(paste(toMatch ,collapse="|"), game_messages)]

  
  start_game_timestamp <- as.numeric(str_filter(game_messages[grep('newGame', game_messages)], 'time = ([[:digit:]]+)')[[1]][[2]])
  end_game_timestamp <- as.numeric(str_filter(game_messages[grep('gameOver', game_messages)], 'time = ([[:digit:]]+)')[[1]][[2]])
  
  events_timestamps <-  sapply(str_filter(game_messages, 'time = ([[:digit:]]+)'), function(i) (as.numeric(i[[2]])))
  move_messages <- game_messages[grep(paste(c("ballMove", "ballSelect", "ballDeselect", "blockedMove"),collapse="|"),game_messages)]
  move_messages <- str_filter(move_messages, 'type\":\"([[:alpha:]]+).+time = ([[:digit:]]+)')
  move_messages <- data.frame(
    time = sapply(move_messages, function(x) as.numeric(x[[3]])),
    event = sapply(move_messages, function(x) x[[2]])
  )
  move_durations <- vector()
  n <- 1
  for (i in 1:nrow(move_messages)-1)
  {
    if(move_messages[i,]$event == 'ballSelect' && (move_messages[i+1,]$event == "ballMove" || move_messages[i+1,]$event == "blockedMove")){
      move_durations[n] = move_messages[i+1,]$time - move_messages[i,]$time
      n <- n + 1
    }
  }
  events_timestamps <- unique(events_timestamps[events_timestamps >= 0 &
                                                  events_timestamps >= start_game_timestamp &
                                                  events_timestamps <= end_game_timestamp ])
  
  scheme <- generate_game_scheme(game_messages, events_timestamps)
  list(scheme = scheme, move_durations = move_durations,
       events_timestamps = events_timestamps, game_messages = game_messages)
}

# meaningful messages types:
# - ballCreate
# - ballSelect
# - ballMove
# - ballRemove



generate_game_scheme <- function(game_messages, events_timestamps){
  game_states <- list()
  game_list_timestamps <- sapply(str_filter(game_messages, 'time = ([[:digit:]]+)'), function(i) (as.numeric(i[[2]])))
  for (i in 1:length(events_timestamps)){
    if(i == 1){
      m <- matrix(0, nrow = 9, ncol = 9)
    }
    actual_messages <- game_messages[which(game_list_timestamps %in% events_timestamps[i])]
    for ( ii in 1:length(actual_messages)){
      e <- str_filter(actual_messages[ii], 'type\\":\\"([[:alpha:]]+)')[[1]][2]
      if(e == "ballCreate"){
        params <- as.numeric(unlist(str_filter(actual_messages[ii], 'color\\":([[:digit:]]),\\"index\\":([[:digit:]]+)'))[c(2,3)])
        m[params[2]+1] <- params[1]
      } else if (e == "ballRemove"){
        index <- as.numeric(unlist(str_filter(actual_messages[ii], 'index\\":([[:digit:]]+)'))[c(2)])+1
        m[index] <- 0
      } else if(e == "ballSelect"){
        if(any(m > 100)){
          m[which(m > 100)] <- m[which(m > 100)] - 100
        }
        index <- as.numeric(unlist(str_filter(actual_messages[ii], 'index\\":([[:digit:]]+)'))[c(2)])+1
        m[index] <- m[index] + 100
      } else if(e == "ballDeselect"){
        if(any(m > 100)){
          m[which(m > 100)] <- m[which(m > 100)] - 100
        }
      } else if(e == "ballMove"){
        params <- as.numeric(unlist(str_filter(actual_messages[ii], 'from\\":([[:digit:]]+),\\"to\\":([[:digit:]]+)'))[c(2,3)])
        from_pos <- params[1]+1
        to_pos <- params[2]+1
        color <- m[from_pos] - 100
        m[from_pos] <- 0
        m[to_pos] <- color
      }
    }
    game_states[[i]] <- m
    if(i < length(events_timestamps)){
      game_states[[i+1]] <- m
    }
  }
  game_states
}

get_position <- function(index) {
  elemCol <- max(index %% 9, 1)
  elemRow <- max(index / 9, 1)
  c(elemRow, elemCol)
}
