game_state_recoverer <- function(eyetracking_data, field_width, field_height)
{
  lines <- eyetracking_data$events$message
  first_sync <- eyetracking_data$sync_timestamp
  
  def <- function(x) if(is.null(x)) NaN else as.numeric(x)
  
  game <- do.call(rbind, lapply(str_filter(lines, 'gm (\\{.+\\}), time = ([0-9]+)'), function(X){
    json <- fromJSON(X[[2]])
    
    data.frame(
      type=json$type,
      time= as.numeric(X[[3]])-first_sync,
      from = def(json$from),
      to = def(json$to),
      color = def(json$color),
      index = def(json$index)
    )
  }))
  
  start_game_timestamp <- game$time[game$type=="newGame"]
  end_game_timestamp <- game$time[game$type=="gameOver"]
  
  game <- game[ game$time>=start_game_timestamp & game$time <= end_game_timestamp ,]
  

  moves <- game[game$type %in% c('ballSelect', 'ballMove', 'blockedMove'), c('type', 'time')]
  
  move_durations <- c()
  for (i in 1:nrow(moves)-1)
  {
    if(moves$type[i] == 'ballSelect' && (moves$type[i+1] == "ballMove" || moves$type[i+1] == "blockedMove")){
      move_durations <- c(move_durations, moves$time[i+1] - moves$time[i])
    }
  }
  
  
  states <- list()
  times <- c()
  time <- start_game_timestamp
  
  m <- matrix(0, nrow = field_height, ncol = field_width)
  
  by(game, 1:nrow(game), function(move){
    
    if(move$type == "ballCreate"){
      m[move$index+1] <<- move$color
    } else if (move$type == "ballRemove"){
      m[move$index+1] <<- 0
    } else if(move$type == "ballSelect"){
      m[which(m > 100)] <<- m[which(m > 100)] - 100
      m[move$index+1] <<- m[move$index+1] + 100
    } else if(move$type == "ballDeselect"){
      m[which(m > 100)] <<- m[which(m > 100)] - 100
    } else if(move$type == "ballMove"){
      from_pos <- move$from+1
      to_pos <- move$to+1
      color <- m[from_pos] - 100
      m[from_pos] <<- 0
      m[to_pos] <<- color
    } else if(move$type %in% c('BoardPositionClicked', 'gameOver')) {
      
      states <<- c(states, list(t(m)))
      times <<- c(times, time)
      time <<- move$time
    }
  })

  list(states = states, move_durations = move_durations,
       times = times, moves=game)
}

# meaningful messages types:
# - ballCreate
# - ballSelect
# - ballMove
# - ballRemove



generate_game_scheme <- function(game, field_width, field_height){
  
}
