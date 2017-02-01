get_eye_actions <- function(filename) {
  
  #clean replica of the extract.actions() from hybridEyeEEG
  
  ans <- load.one.eye(filename)
  
  lines <- ans$events$message
  first_sync <- ans$sync_timestamp
  def <- function(x) if(is.null(x)) NaN else as.numeric(x)
  fixation_duration <- as.numeric((str_filter(lines, '"fixationDuration\":([[:digit:]]+)'))[[1]][[2]])
  quick_fixation_duration <- as.numeric((str_filter(lines, '"quickFixationDuration\":([[:digit:]]+)'))[[1]][[2]])
  
  game <- do.call(rbind, lapply(str_filter(lines, 'gm (\\{.+\\}), time = ([0-9]+)'), function(X){
    json <- fromJSON(X[[2]])
    data.frame(
      time= (as.numeric(X[[3]]) - first_sync - fixation_duration),
      type=json$type
    )
  }))
  
  fix_times <- do.call(rbind, lapply(str_filter(lines, 'fixation in region.+time += ([0-9]+)'), function(X){
    data.frame(
      time= (as.numeric(X[[2]]) - first_sync - fixation_duration),
      type='fixation'
    )
  }))
  
  df <- rbind(game, fix_times, data.frame(time = c(ans$SRstartFix, ans$SRstartSacc, ans$SRendSacc), 
                                          type = c(rep('SRfix', length(ans$SRstartFix)),
                                                   rep('SRsaccStart', length( ans$SRstartSacc)),
                                                   rep('SRSaccEnd', length(ans$SRendSacc)))
                                          ))
  
  
  df$time <- df$time/1000
  df <- df[order(df$time),]
  
  
  for (i in 1:(nrow(df)-1))
  {
    if (df$type[i] == "msgballChosen" 
        && df$type[i+1] == "msgballChosen" )
    {
      df$type[i] = "errorBallChosen" 
    }
  }
  
  df <- subset(df, time >=0)
  
  colnames(df) <- c("Latency", "Type")
  
  return(df)
  
}