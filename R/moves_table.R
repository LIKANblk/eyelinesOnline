moves_table <- function(experiment) {
  
  n_train <- 0
  n_normal <- 0
  n_random <- 0
  N_moves <- 0
  short_normal <- 0
  change_short_normal <- 0
  FA_short_normal <- 0
  long_normal <- 0
  change_long_normal <- 0
  FA_long_normal <- 0
  short_random <- 0
  change_short_random <- 0
  FA_short_random <- 0
  long_random <- 0
  change_long_random <- 0
  FA_long_random <- 0
  
  
  for (i in 1:length(experiment)) {
    events <- experiment[[i]]$events
    if(experiment[[i]]$file_data$record_type == 'train') {
      
      n_train <- n_train + 1
      
      N_moves <- N_moves + nrow(events)
      
    } else if (experiment[[i]]$file_data$record_type == 'test') {
      
      n_normal <- n_normal + 1
      
      short_normal <- short_normal + sum(events$quick_fixation == T 
                          & events$activation == T
                          & events$changed_selection == F
                          & events$false_alarm == F)
      change_short_normal <- change_short_normal + sum(events$quick_fixation == T 
                          & events$activation == T
                          & events$changed_selection == T
                          & events$false_alarm == F)
      FA_short_normal <- FA_short_normal + sum(events$quick_fixation == T 
                           & events$activation == T
                           & events$changed_selection == F
                           & events$false_alarm == T)
      
      
    ################## LONG FIXATIONS #####################
      
      long_normal <- long_normal + sum(events$quick_fixation == F 
                                         & events$activation == T
                                         & events$changed_selection == F
                                         & events$false_alarm == F)
      change_long_normal <- change_long_normal + sum(events$quick_fixation == F 
                                           & events$activation == T
                                           & events$changed_selection == T
                                           & events$false_alarm == F)
      FA_long_normal <- FA_long_normal + sum(events$quick_fixation == F 
                                   & events$activation == T
                                   & events$changed_selection == F
                                   & events$false_alarm == T)
      
    } else if (experiment[[i]]$file_data$record_type == 'random') {
      
      n_random <- n_random + 1
      
      short_random <- short_random + sum(events$quick_fixation == T 
                          & events$activation == T
                          & events$changed_selection == F
                          & events$false_alarm == F)
      change_short_random <- change_short_random + sum(events$quick_fixation == T 
                           & events$activation == T
                           & events$changed_selection == T
                           & events$false_alarm == F)
      FA_short_random <- FA_short_random + sum(events$quick_fixation == T 
                       & events$activation == T
                       & events$changed_selection == F
                       & events$false_alarm == T)
      
      ################## LONG FIXATIONS #####################
      
      long_random <- long_random + sum(events$quick_fixation == F 
                                       & events$activation == T
                                       & events$changed_selection == F
                                       & events$false_alarm == F)
      change_long_random <- change_long_random + sum(events$quick_fixation == F 
                                           & events$activation == T
                                           & events$changed_selection == T
                                           & events$false_alarm == F)
      FA_long_random <- FA_long_random + sum(events$quick_fixation == F 
                                   & events$activation == T
                                   & events$changed_selection == F
                                   & events$false_alarm == T)
    }
  }
  
  df <- data.frame(long_fixations_train = N_moves / n_train,
                   short_normal = short_normal / n_normal,
                   change_short_normal = change_short_normal / n_normal,
                   FA_short_normal = FA_short_normal / n_normal,
                   
                   long_normal = long_normal / n_normal,
                   change_long_normal = change_long_normal / n_normal,
                   FA_long_normal = FA_long_normal / n_normal,
                   
                   short_random = short_random / n_random, 
                   change_short_random = change_short_random / n_random,
                   FA_short_random = FA_short_random / n_random,
                   
                   long_random = long_random / n_random, 
                   change_long_random = change_long_random / n_random,
                   FA_long_random = FA_long_random / n_random)
  
}