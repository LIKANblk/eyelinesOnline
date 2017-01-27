task_processing <- function(path, type = 'test') {
  
  load(path)
  
  for(i in 1:length(experiment)) {
    
    if( experiment[[i]]$file_data$record_type == 'task_test' && type == 'test') {
      
      clf_resp <- count_clf_resp_task(experiment[[i]]$events)
      
    } else if (experiment[[i]]$file_data$record_type == 'task_random' && type == 'random') {
      
      clf_resp <- count_clf_resp_task(experiment[[i]]$events)
      
    }
  }
  
  clf_resp
}

count_clf_resp_task <- function(events) {
  
  eye <- load.one.eye(experiment[[i]]$file_data$filename_edf)
  
  if( (tail(events$time[which(events$field_type == 'field')], n=1) + 90000) >= events$time[nrow(events)] ) {
    warning(paste0(experiment[[i]]$file_data$filename_edf, ' is too short for a proper analysis!'))
    return()
  }
  
  events_sequence_task <- events[1:which(events$time == tail(events$time[which(events$field_type == 'field')], n=1)) ,]
  events_remember_task <- events[which(events$time > eye$samples$time[length(eye$samples$time)] - 90000)[1]:nrow(events),]
  
  quick_fix_and_activation_seq <- sum(events_sequence_task$quick_fixation == TRUE & events_sequence_task$activation == TRUE) 
  long_fix_and_activation_seq <- sum(events_sequence_task$quick_fixation == FALSE & events_sequence_task$activation == TRUE)
  quick_fix_no_activation_seq <- sum(events_sequence_task$quick_fixation == TRUE & events_sequence_task$activation == FALSE) 
  long_fix_no_activation_seq <- sum(events_sequence_task$quick_fixation == FALSE & events_sequence_task$activation == FALSE)
  
  quick_fix_and_activation_rem <- sum(events_remember_task$quick_fixation == TRUE & events_remember_task$activation == TRUE) 
  long_fix_and_activation_rem <- sum(events_remember_task$quick_fixation == FALSE & events_remember_task$activation == TRUE)
  quick_fix_no_activation_rem <- sum(events_remember_task$quick_fixation == TRUE & events_remember_task$activation == FALSE) 
  long_fix_no_activation_rem <- sum(events_remember_task$quick_fixation == FALSE & events_remember_task$activation == FALSE)
  
  df <- data.frame(
    quick_fix_and_activ_seq = quick_fix_and_activation_seq,
    long_fix_and_activ_seq = long_fix_and_activation_seq,
    quick_fix_no_activ_seq = quick_fix_no_activation_seq,
    long_fix_no_activ_seq = long_fix_no_activation_seq,
    quick_fix_and_activ_rem = quick_fix_and_activation_rem,
    long_fix_and_activ_rem = long_fix_and_activation_rem,
    quick_fix_no_activ_rem = quick_fix_no_activation_rem,
    long_fix_no_activ_rem = long_fix_no_activation_rem
  )
  df
}