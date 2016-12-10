prepare_results_clf <- function(path, ball_only = F) {
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
  
  if(ball_only) {
    test_table <- test_table[test_table$field_type == 'ball', ]
    random_table <- random_table[random_table$field_type == 'ball', ]
  }
  
  normal_TP <- sum(test_table$activation == T & test_table$quick_fixation == T)
  normal_TN <- sum(test_table$activation == F & test_table$quick_fixation == T)
  normal_FP <- sum(test_table$false_alarm == T)
  normal_FN <- sum(test_table$activation == T & test_table$dwell_time == 1000)
  
  
  random_TP <- sum(random_table$activation == T & random_table$quick_fixation == T)
  random_TN <- sum(random_table$activation == F & random_table$quick_fixation == T)
  random_FP <- sum(random_table$false_alarm == T)
  random_FN <- sum(random_table$activation == T & random_table$dwell_time == 1000)
  
  df_fo_plot_test <- data.frame(type = c('TP', 'TN', 'FP', 'FN'),
                                clf_count = c(normal_TP, normal_TN, normal_FP, normal_FN))
  df_fo_plot_random <- data.frame(type = c('TP', 'TN', 'FP', 'FN'), 
                                  clf_count = c(random_TP, random_TN, random_FP, random_FN))
  l <- list(normal = df_fo_plot_test, random = df_fo_plot_random)
  l
}