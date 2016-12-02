table_sens_spec <- function() {
  
  path <- '/home/mayenok/Yandex.Disk/eyelinesOnlineNew/data/'
  
  ball_sens_test <- c()
  cell_sens_test <- c()
  ball_spec_test <- c()
  cell_spec_test <- c()
  
  ball_sens_random <- c()
  cell_sens_random <- c()
  ball_spec_random <- c()
  cell_spec_random <- c()
  
  for (k in c(10:12,14:27)) {
    events_test <- data.frame()
    events_random <- data.frame()
    
    load(paste0(path, k, '/experiment.RData'))
    
    for ( i in 1:length(experiment)) {
      if(experiment[[i]]$file_data$record_type == 'test') {
        events_test <- rbind(events_test, experiment[[i]]$events)
      }
    }
    
    l <- get_clf_results(events_test)
    
    ball_sens_test <- c(ball_sens_test, l$ball_sens)
    cell_sens_test <- c(cell_sens_test, l$cell_sens)
    ball_spec_test <- c(ball_spec_test, l$ball_spec)
    cell_spec_test <- c(cell_spec_test, l$cell_spec)
    
    for ( i in 1:length(experiment)) {
      if(experiment[[i]]$file_data$record_type == 'random') {
        events_random <- rbind(events_random, experiment[[i]]$events)
      }
    }
    
    l <- get_clf_results(events_random)
    
    ball_sens_random <- c(ball_sens_random, l$ball_sens)
    cell_sens_random <- c(cell_sens_random, l$cell_sens)
    ball_spec_random <- c(ball_spec_random, l$ball_spec)
    cell_spec_random <- c(cell_spec_random, l$cell_spec)
    
    remove(experiment)
  }
  df <- data.frame(ball_sens_test = ball_sens_test,
                   cell_sens_test = cell_sens_test,
                   ball_spec_test = ball_spec_test,
                   cell_spec_test = cell_spec_test,
                   ball_sens_random = ball_sens_random,
                   cell_sens_random = cell_sens_random,
                   ball_spec_random = ball_spec_random,
                   cell_spec_random = cell_spec_random)
  df
}


get_clf_results <- function(events) {
  TP_ball <- length(which(events$quick_fixation == T & events$activation == T & events$field_type == 'ball'))
  TP_cell <- length(which(events$quick_fixation == T & events$activation == T & events$field_type == 'field'))
  TN_ball <- length(which(events$quick_fixation == T & events$activation == F & events$field_type == 'ball'))
  TN_cell <- length(which(events$quick_fixation == T & events$activation == F & events$field_type == 'field'))
  FP_ball <- length(which(events$false_alarm == T & events$field_type == 'ball'))
  FP_cell <- length(which(events$false_alarm == T & events$field_type == 'field'))
  FN_ball <- length(which(events$quick_fixation == F & events$activation == T & events$field_type == 'ball'))
  FN_cell <- length(which(events$quick_fixation == F & events$activation == T & events$field_type == 'field'))
  
  SENS = c((TP_ball / (TP_ball + FN_ball)), (TP_cell / (TP_cell + FN_cell)))
  SPEC = c((1 - (FP_ball / (TN_ball + FP_ball))), (1 - (FP_cell / (TN_cell + FP_cell))))
  
  ball_sens <- SENS[1]
  cell_sens <- SENS[2]
  ball_spec <- SPEC[1]
  cell_spec <- SPEC[2]
  
  l <- list(ball_sens = as.numeric(format(ball_sens, digits = 3)),
            cell_sens = as.numeric(format(cell_sens, digits = 3)),
            ball_spec = as.numeric(format(ball_spec, digits = 3)),
            cell_spec = as.numeric(format(cell_spec, digits = 3)))
}