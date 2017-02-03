get_report_summary <- function(normal_table, random_table, count_changed_selection = T, no_false_alarms = F) {
  
  
  TP_ball_normal <- sum(normal_table$quick_fixation == T & normal_table$activation == T & normal_table$changed_selection == F & normal_table$false_alarm == F)
  TN_ball_normal <-sum(normal_table$quick_fixation == T & normal_table$activation == F  & normal_table$changed_selection == F  & normal_table$false_alarm == F)
  if(count_changed_selection) {
    FP_ball_normal <-sum((normal_table$false_alarm == T | normal_table$changed_selection == T ) & normal_table$quick_fixation == T)
  } else if(no_false_alarms) {
    FP_ball_normal <-sum(normal_table$changed_selection == T & normal_table$quick_fixation == T)
  } else {
    FP_ball_normal <-sum(normal_table$false_alarm == T  & normal_table$quick_fixation == T)
  }
  
  FN_ball_normal <- sum(normal_table$quick_fixation == F & normal_table$activation == T & normal_table$changed_selection == F  & normal_table$false_alarm == F)
  
  SENS_normal <- TP_ball_normal / (TP_ball_normal + FN_ball_normal)
  SPEC_normal <- 1 - (FP_ball_normal / (TN_ball_normal + FP_ball_normal))
  J_normal <- (SENS_normal + SPEC_normal) - 1
  
  #################################################
  
  TP_ball_random <- sum(random_table$quick_fixation == T & random_table$activation == T & random_table$changed_selection == F  & random_table$false_alarm == F)
  TN_ball_random <- sum(random_table$quick_fixation == T & random_table$activation == F & random_table$changed_selection == F & random_table$false_alarm == F)
  if(count_changed_selection) {
    FP_ball_random <- sum((random_table$false_alarm == T | random_table$changed_selection == T ) & random_table$quick_fixation == T)
  } else if(no_false_alarms) {
    FP_ball_random <-sum(random_table$changed_selection == T & random_table$quick_fixation == T)
  } else {
    FP_ball_random <- sum(random_table$false_alarm == T  & random_table$quick_fixation == T)
  }
  FN_ball_random <- sum(random_table$quick_fixation == F & random_table$activation == T & random_table$changed_selection == F & random_table$false_alarm == F)
  
  SENS_random <- TP_ball_random / (TP_ball_random + FN_ball_random)
  SPEC_random <- 1 - (FP_ball_random / (TN_ball_random + FP_ball_random))
  J_random <- (SENS_random + SPEC_random) - 1
  
  df <- data.frame(SENS_normal = SENS_normal, SPEC_normal = SPEC_normal, J_normal = J_normal,
                    chngn_normal_rate = sum(normal_table$changed_selection == TRUE & normal_table$quick_fixation == T) / TP_ball_normal * 100,
                    #FP_normal_rate = sum(normal_table$false_alarm == TRUE & normal_table$quick_fixation == F) / TP_ball_normal * 100,
                    SENS_random = SENS_random, SPEC_random = SPEC_random, J_random = J_random,
                    chngn_random_rate = sum(random_table$changed_selection == TRUE & random_table$quick_fixation == T) / TP_ball_random * 100#,
                    #FP_random_rate = sum(random_table$false_alarm == TRUE & random_table$quick_fixation == F) / TP_ball_random * 100
                   )

  df
  
}