results_for_tables <- function(experiments_nums) {
  
  #experiments_nums example results_for_tables(c(29,30,32,33)) or results_for_tables(23:27)
  
  # for (i in c(23:27, 29, 30, 32:35)) {
  #   process_experiment(paste0('~/Yandex.Disk/eyelinesOnlineNew/data/', i, '/'))
  #   generate_files_for_matlab(paste0('~/Yandex.Disk/eyelinesOnlineNew/data/', i, '/experiment.RData'))
  # }
  
  ################ TABLE 1  ################ 
  
  report_summary <- data.frame()
  for (i in experiments_nums) {
    load(paste0('~/Yandex.Disk/eyelinesOnlineNew/data/', i, '/experiment.RData'))
    l <- bar(experiment)
    report_summary <- rbind(report_summary, get_report_summary(l$normal_table, l$random_table, FALSE))
    
    remove(experiment)
  }
  
  report_summary$exp_name <- experiments_nums
  write.table(format(report_summary, digits = 2), file="/home/mayenok/Yandex.Disk/eyelinesOnlineNew/tables/report_summary_no_change.csv", row.names = F)
  
  
  ################ TABLE 2 ################ 
  results_clf <- data.frame()
  for (i in experiments_nums) {
    load(paste0('~/Yandex.Disk/eyelinesOnlineNew/data/', i, '/experiment.RData'))
    l <- bar(experiment)
    results_clf <- rbind(results_clf, prepare_results_clf(l$normal_table, l$random_table, l$n_test, l$n_random))
    
    remove(experiment)
  }
  
  results_clf$exp_name <- experiments_nums
  write.table(format(results_clf, digits = 2), file="/home/mayenok/Yandex.Disk/eyelinesOnlineNew/tables/results_clf.csv", row.names = F)
  
  
  ################ TABLE 3 ################ 
  moves_table <- data.frame()
  n_fixes <- c()
  for (i in experiments_nums) {
    load(paste0('~/Yandex.Disk/eyelinesOnlineNew/data/', i, '/experiment.RData'))
    l <- bar(experiment)
    moves_table <- rbind(moves_table, moves_table(l$normal_table, l$random_table, l$n_test, l$n_random))
    n_fixes_in_train <- 0
    
    for(i in 1:length(experiment)){
      if(experiment[[i]]$file_data$record_type == 'train'){
        n_fixes_in_train <- n_fixes_in_train + nrow(experiment[[i]]$events[
          (experiment[[i]]$events$field_type == 'ball' | experiment[[i]]$events$field_type == 'field') &
            experiment[[i]]$events$impossible_move == F,])
      }
    }
    n_fixes <- c(n_fixes, n_fixes_in_train)
    remove(experiment)
  }
  
  moves_table$exp_name <- experiments_nums
  moves_table$n_target_fixes_train <- n_fixes
  write.table(format(moves_table, digits = 2), file="/home/mayenok/Yandex.Disk/eyelinesOnlineNew/tables/moves_table.csv", row.names = F)
  
}

bar <- function(experiment) {
  ######################### PREPARE DATA FRAMES FO ANALYSIS ######################### 
  normal_table <- data.frame()
  random_table <- data.frame()
  
  n_test <- 0
  n_random <- 0
  
  n_train <- 0
  N_fixes <- 0
  
  for ( i in 1:length(experiment)) {
    if(experiment[[i]]$file_data$record_type == 'test') {
      normal_table <- rbind(normal_table, experiment[[i]]$events[experiment[[i]]$events$field_type == 'ball', ])
      n_test <- n_test + 1
    } else if (experiment[[i]]$file_data$record_type == 'random'){
      random_table <- rbind(random_table, experiment[[i]]$events[experiment[[i]]$events$field_type == 'ball', ])
      n_random <- n_random + 1
    } else if (experiment[[i]]$file_data$record_type == 'train') {
      N_fixes <- N_fixes + nrow(experiment[[i]]$events[experiment[[i]]$events$field_type == 'ball',])
      n_train <- n_train + 1
    }
  }
  
  l <- list(normal_table = normal_table, 
            random_table = random_table, 
            n_test = n_test, 
            n_random = n_random,
            N_fixes = N_fixes / n_train)
}
