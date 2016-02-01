prepare <- function(M){
  
  RM <- diag(nrow=32)[1:13,]
  
  
  src <- source.channels(M, 500, 10000);
  
  less <- pipe.spatial(src, RM)
  cl <- signalPreparation(less, 1, high=70) 
  
  out <- drain.channelsRecorder(
    cl
    #  signalPreparation(, 1, high=100) 
  )
  pump(src)
  
  f <- out()
  f <- cbind(f, M[,32])
  f[,13] = 0
  f[which(diff(M[,32])>0), 13]=1
  f
}