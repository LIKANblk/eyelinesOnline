extract_clf_results <- function(path){
  load(path)
  
  test_table <- data.frame()
  random_table <- data.frame()
  
  for ( i in 1:length(experiment)) {
    if(experiment[[i]]$file_data$record_type == 'test') {
      test_table <- rbind(test_table, experiment[[i]]$events)
    }
  }
  
  for ( i in 1:length(experiment)) {
    if(experiment[[i]]$file_data$record_type == 'random') {
      random_table <- rbind(random_table, experiment[[i]]$events)
    }
  }
  
  normal_TP <- sum(test_table$activation == T & test_table$quick_fixation == T)
  normal_TN <- sum(test_table$activation == F & test_table$quick_fixation == T)
  normal_FP <- sum(test_table$false_alarm == T)
  normal_FN <- sum(test_table$activation == T & test_table$dwell_time == 1000)
  
  
  random_TP <- sum(random_table$activation == T & random_table$quick_fixation == T)
  random_TN <- sum(random_table$activation == F & random_table$quick_fixation == T)
  random_FP <- sum(random_table$false_alarm == T)
  random_FN <- sum(random_table$activation == T & random_table$dwell_time == 1000)
  
  df_for_table <- data.frame(normal_TP = normal_TP, normal_TN = normal_TN,
                             normal_FP = normal_FP, normal_FN = normal_FN,
                             random_TP = random_TP, random_TN = random_TN,
                             random_FP = random_FP, random_FN = random_FN)
  kable(df_for_table, align = 'r', digits = 3, padding = 2)
  
  remove(experiment)
  l <- kable(df_for_table, align = 'r', digits = 3, padding = 2)
  l
  
}