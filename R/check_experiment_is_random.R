check_experiment_is_random <- function(exp){
  data <- R3::readStructurized(exp$file_data$filename_r2e)
  classifier <- readChar(exp$file_data$filename_classifier, file.info(exp$file_data$filename_classifier)$size)
  classifier <- strRep(classifier, '\r', '')
  
  result <- run.offline(data$streams, data$blocks, classifier)
  
  targId <- Filter(function(x) x$name=='RES', data$streams)[[1]]$id
  
  target <- Filter(function(x) { SI(x)$id == targId }, data$blocks)
  
  S1 <- sapply(target, as.character)
  S2 <- sapply(result$RES, as.character)
  
  list(length(target)!=length(result$RES) || any(S1!=S2), S1, S2)
}