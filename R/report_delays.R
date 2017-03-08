report_delays <- function(file){
  
  data <- readStructurized(file)
  click <- Filter(function(x) SI(x)$id == 1, data$blocks)
  res <- Filter(function(x) SI(x)$id == 2, data$blocks)
  clc <- sapply(res, as.character)
  src <- Filter(function(x) as.character(x) %in% clc, click)
  print(summary( (sapply(click, attr, 'received') - sapply(click, attr, 'created'))/1E3  ))
  print(summary( (sapply(res, attr, 'created') - sapply(src, attr, 'received'))/1E3 ))

  
}