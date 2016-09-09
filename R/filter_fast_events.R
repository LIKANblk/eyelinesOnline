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
      for(ev in events){
        if(!is.null(attr(ev, 'TS')) && attr(ev, 'TS')-lastTS >=barrier){
          lastTS <<- attr(ev, 'TS')
          ret <- c(ret, list(ev))
        }
      }
      
      ret
    }
  )
}