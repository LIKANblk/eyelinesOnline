moves_table <- function(normal_table, random_table, n_normal, n_random) {
  
  
  
  short_normal <-  sum(normal_table$quick_fixation == T & normal_table$activation == T & normal_table$changed_selection == F   & normal_table$false_alarm == F)
  change_short_normal <-  sum(normal_table$quick_fixation == T  & normal_table$changed_selection == T & normal_table$false_alarm == F)
  FA_short_normal <-  sum(normal_table$quick_fixation == T & normal_table$changed_selection == F & normal_table$false_alarm == T)
  
  
  ################## LONG FIXATIONS #####################
  
  long_normal <-  sum(normal_table$quick_fixation == F 
                                   & normal_table$activation == T
                                   & normal_table$changed_selection == F
                                   & normal_table$false_alarm == F)
  change_long_normal <-  sum(normal_table$quick_fixation == F 
                                                 & normal_table$changed_selection == T
                                                 & normal_table$false_alarm == F)
  FA_long_normal <- sum(normal_table$quick_fixation == F 
                                         & normal_table$changed_selection == F
                                         & normal_table$false_alarm == T)
  
  
  
  ################## RANDOM #####################
  
  short_random <- sum(random_table$quick_fixation == T  & random_table$activation == T & random_table$changed_selection == F & random_table$false_alarm == F)
  change_short_random <-  sum(random_table$quick_fixation == T & random_table$changed_selection == T                                                  & random_table$false_alarm == F)
  FA_short_random <- sum(random_table$quick_fixation == T  & random_table$changed_selection == F & random_table$false_alarm == T)
  
  ################## LONG FIXATIONS #####################
  
  long_random <-  sum(random_table$quick_fixation == F 
                                   & random_table$activation == T
                                   & random_table$changed_selection == F
                                   & random_table$false_alarm == F)
  change_long_random <-  sum(random_table$quick_fixation == F 
                                                 & random_table$changed_selection == T
                                                 & random_table$false_alarm == F)
  FA_long_random <-  sum(random_table$quick_fixation == F 
                                         & random_table$changed_selection == F
                                         & random_table$false_alarm == T)
  
  
  df <- data.frame(short_normal = short_normal / n_normal,
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
  
  df
  
}