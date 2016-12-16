prepare_results_clf <- function(normal_table, random_table, n_normal, n_random) {
  
  normal_TP <- sum(normal_table$activation == T & normal_table$quick_fixation == T & normal_table$changed_selection == F & normal_table$false_alarm == F)
  normal_TN <- sum(normal_table$activation == F & normal_table$quick_fixation == T & normal_table$changed_selection == F & normal_table$false_alarm == F)
  normal_FP <- sum((normal_table$false_alarm == T | normal_table$changed_selection == T) & normal_table$quick_fixation == T) 
  normal_FN <- sum(normal_table$activation == T & normal_table$dwell_time == 1000 & normal_table$changed_selection == F & normal_table$false_alarm == F)
  
  
  random_TP <- sum(random_table$activation == T & random_table$quick_fixation == T & random_table$changed_selection == F & random_table$false_alarm == F) 
  random_TN <- sum(random_table$activation == F & random_table$quick_fixation == T & random_table$changed_selection == F & random_table$false_alarm == F)
  random_FP <- sum((random_table$false_alarm == T | random_table$changed_selection == T) & random_table$quick_fixation == T)
  random_FN <- sum(random_table$activation == T & random_table$dwell_time == 1000 & random_table$changed_selection == F & random_table$false_alarm == F)
  
  df <- data.frame(normal_TP = normal_TP / n_normal, normal_TN = normal_TN / n_normal, normal_FP = normal_FP / n_normal, normal_FN = normal_FN / n_normal,
                   random_TP = random_TP / n_random, random_TN = random_TN / n_random, random_FP = random_FP / n_random, random_FN = random_FN / n_random)
  df
}