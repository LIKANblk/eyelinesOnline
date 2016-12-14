get_report_summary <- function(experiment) {
  
  scores <- 0
  random_records <- 0
  normal_records <- 0
  all_records <- 0
  normal_table <- data.frame()
  random_table <- data.frame()
  
  for ( i in 1:length(experiment)) {
    
    scores <- scores + experiment[[i]]$file_data$score
    all_records <- all_records + 1
    if(experiment[[i]]$file_data$record_type == 'test') {
      normal_table <- rbind(normal_table, experiment[[i]]$events)
      normal_records <- normal_records + 1
    } else if (experiment[[i]]$file_data$record_type == 'random'){
      random_table <- rbind(random_table, experiment[[i]]$events)
      random_records <- random_records + 1
    }
  }
  
  TP_ball_normal <- length(which(normal_table$quick_fixation == T & normal_table$activation == T 
                                 & normal_table$field_type == 'ball' & normal_table$changed_selection == F))
  TN_ball_normal <- length(which(normal_table$quick_fixation == T & normal_table$activation == F 
                                 & normal_table$field_type == 'ball' & normal_table$changed_selection == F))
  FP_ball_normal <- length(which((normal_table$false_alarm == T | normal_table$changed_selection == T) & normal_table$field_type == 'ball'))
  FN_ball_normal <- length(which(normal_table$quick_fixation == F & normal_table$activation == T 
                                 & normal_table$field_type == 'ball' & normal_table$changed_selection == F))
  SENS_normal <- TP_ball_normal / (TP_ball_normal + FN_ball_normal)
  SPEC_normal <- 1 - (FP_ball_normal / (TN_ball_normal + FP_ball_normal))
  J_normal <- (SENS_normal + SPEC_normal) - 1
  #################################################
  TP_ball_random <- length(which(random_table$quick_fixation == T & random_table$activation == T 
                                 & random_table$field_type == 'ball' & random_table$changed_selection == F))
  TN_ball_random <- length(which(random_table$quick_fixation == T & random_table$activation == F 
                                 & random_table$field_type == 'ball' & random_table$changed_selection == F))
  FP_ball_random <- length(which((random_table$false_alarm == T | random_table$changed_selection == T) & random_table$field_type == 'ball'))
  FN_ball_random <- length(which(random_table$quick_fixation == F & random_table$activation == T 
                                 & random_table$field_type == 'ball' & random_table$changed_selection == F))
  SENS_random <- TP_ball_random / (TP_ball_random + FN_ball_random)
  SPEC_random <- 1 - (FP_ball_random / (TN_ball_random + FP_ball_random))
  J_random <- (SENS_random + SPEC_random) - 1
  
  tmp <- data.frame(sbj = ii, scores = scores / all_records, SENS_normal = SENS_normal, SPEC_normal = SPEC_normal, J_normal = J_normal,
                    chngn_normal_rate = sum(normal_table$changed_selection == TRUE & normal_table$field_type == 'ball') / TP_ball_normal * 100,
                    FP_normal_rate = sum(normal_table$false_alarm == TRUE & normal_table$field_type == 'ball') / TP_ball_normal * 100,
                    SENS_random = SENS_random, SPEC_random = SPEC_random, J_random = J_random,
                    chngn_random_rate = sum(random_table$changed_selection == TRUE & random_table$field_type == 'ball') / TP_ball_random * 100,
                    FP_random_rate = sum(random_table$false_alarm == TRUE & random_table$field_type == 'ball') / TP_ball_random * 100)

  tmp
  
}