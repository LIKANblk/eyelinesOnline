number_of_evs <- function(DF, name){
  ids <- which(DF$type==name)
  ids <- ids[ids>1]
  
  ids <- ids[DF$type[ids-1]=='fix']
  ids <- ids[(DF$time[ids]-DF$time[ids-1])<0.02]
  length(ids)
}