filter_fast_events <- function(events, tooFast = 1000){
  processor(
    events,
    prepare = function(env){
      env$lastTS <- 0
      env$barrier <- tooFast*1E3
      SI(events)
    },
    online = function(events){
      ret <- list()
      for(event in events){
        if(!is.null(attr(event, 'TS')) && attr(event, 'TS')-lastTS >=barrier){
          lastTS <<- attr(event, 'TS')
          ret <- c(ret, event)
        }
      }
      
      ret
    }
  )
}