prepare.data <- function(signal, actions, epoch_size, sRate, left_border)
{
  msgbuttonPressed_t <- ceiling(actions[which(actions$Type=="msgbuttonPressed"),1]*sRate)
  msgballChosen_t <- ceiling(actions[which(actions$Type=="msgballChosen"),1]*sRate)
  msgBallMoved_t <- ceiling(actions[which(actions$Type=="msgBallMoved"),1]*sRate)
  msgClickedInBlockMode_t <- ceiling(actions[which(actions$Type=="msgClickedInBlockMode"),1]*sRate)
  msgBallClickedInBlockedMode_t <- ceiling(actions[which(actions$Type=="msgBallClickedInBlockedMode"),1]*sRate)
  msgBoardClickedInBlockedMode_t <- ceiling(actions[which(actions$Type=="msgBoardClickedInBlockedMode"),1]*sRate)
  
  epoch_size <- round((epoch_size/1000) * sRate)
  
  #eventsT_t = c(msgbuttonPressed_t, msgballChosen_t, msgBallMoved_t)
  eventsT_t = msgbuttonPressed_t
  #eventsNT_t = c(msgClickedInBlockMode_t, msgBallClickedInBlockedMode_t, msgBoardClickedInBlockedMode_t)
  eventsNT_t = msgBallClickedInBlockedMode_t
  
  eventsNT_t <- eventsNT_t[eventsNT_t<=nrow(signal)]
  eventsT_t <- eventsT_t[eventsT_t<=nrow(signal)]
  
  cat("Train epochs: target=", length(eventsT_t), " nontarget=", length(eventsNT_t), "\n")
  
  nChannels <- dim(signal)[2]
  left_border = left_border/1000*sRate
  
  eegT <- array(dim = c(epoch_size, nChannels-1, length(eventsT_t)))
  
  for (i in 1:length(eventsT_t))
  {
    eegT[ , , i] <- signal[(eventsT_t[i]+left_border+1):(eventsT_t[i]+epoch_size+left_border), -ncol(signal)]
  }
  
  eegNT = array(dim = c(epoch_size, nChannels-1, length(eventsNT_t)))
  for (i in 1:length(eventsNT_t))
  {
    eegNT[ , , i] <- signal[(eventsNT_t[i]+left_border+1):(eventsNT_t[i]+epoch_size+left_border), -ncol(signal)]
  }
  
  l <- list(eegT = eegT, eegNT = eegNT)
  
  return(l)
  
}