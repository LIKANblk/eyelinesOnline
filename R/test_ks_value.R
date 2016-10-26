test_ks_value <- function(experiment){
  
  data <- c()
  passed <- 0
  total <- 0
  
  lapply(experiment, function(exp){
    if(exp$file_data$record_type == 'test'){
      data <<- c(data, diff(which(exp$eeg_data$classifierOut$passed))-1)
      passed <<- passed + sum(exp$eeg_data$classifierOut$passed)
      total <<- total + nrow(experiment[[8]]$eeg_data$classifierOut)
    }
  })

  y = rnbinom(length(data), 1, passed/total)
  
  KolmogorovSmirnov(data, y)
}