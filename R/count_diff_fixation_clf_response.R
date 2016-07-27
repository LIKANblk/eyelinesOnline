count_diff_fixation_clf_response <- function(filename, file_edf, gap_between_short_fixations = 100,
                                             long_fixation = 2000, short_fixation = 500){
  signal_raw <- R3:::extractChannel(filename, 0)
  
  signal <- load.eeg(filename, channels=c(1:5,7,9:15), low = F, high=5, refs=c(16,17)) 
  sync_mark = attr(signal_raw, 'TS')[which(signal[,dim(signal)[2]] != 0)[3]]
  
  input_timestamps <-  sapply(R3:::extractChannel(filename, 1), function(x){(attr(x, 'TS')-sync_mark)/1E6})
  outut_timestamps <-  sapply(R3:::extractChannel(filename, 2), function(x){(attr(x, 'TS')-sync_mark)/1E6})
  
  x <- data.frame(event = c(rep('input', length(input_timestamps)), rep('output', length(outut_timestamps))),
                  time = c(input_timestamps, outut_timestamps), stringsAsFactors=FALSE)
  x <- x[order(x$time),]
  
  y <- data.frame(event=character(),time=numeric(), stringsAsFactors=FALSE)
  for (i in 1:nrow(x)){
    if(i > 1){
      if(x[i-1,] == "input" && x[i,] == "output") {
        y <- rbind(y, x[i-1,])
        y <- rbind(y, x[i,])
      }
    }
  }
  
  l <- get_game_events(file_edf, filename)
  
  attempts_for_events_move <- create_attempts_for_different_events(y, l$ball.move)
  attempts_for_events_choose <- create_attempts_for_different_events(y,  l$ball.choose)
  
  z <- count_fix_classifier_diffs(x)
  diffs_clf_fix_choose <- choose_inputs(z, attempts_for_events_choose)
  diffs_clf_fix_move <- choose_inputs(z, attempts_for_events_move)
  
  draw_attempts_plot(diffs_clf_fix_choose,long_fixation,gap_between_short_fixations, 'ball_clicked', filename)
  draw_attempts_plot(diffs_clf_fix_move,long_fixation,gap_between_short_fixations, 'empty_field_clicked', filename)
  return (list(attempts_df = x, game_events = l, selected_attempts = z,
               diffs_clf_fix_choose = diffs_clf_fix_choose,
               diffs_clf_fix_move = diffs_clf_fix_move))
}