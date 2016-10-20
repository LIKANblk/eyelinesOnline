test_ks_value <- function(experiment){
  
  data <- do.call(rbind,
                  lapply(
                    Filter(
                      function(x) x$file_data$record_type == 'test' ,
                      experiment
                    ),
                    '[[', 'events'
                  )
  )
  
  H <- hist(data$dwell_time[data$field_type=='ball' & data$classifier_response=='true_positive'], breaks = seq(200,900,100))
  
  
  pbinom(7, 7, 0.13)
  
  ks.test(
    H,
  )
  
}