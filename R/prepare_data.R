prepare.data <- function(signal, actions, epoch_size, sRate, left_border, no_button_press,
                         random_non_target, ball_only, n_random_nontarget)
{
  msgbuttonPressed_t <- ceiling(actions[which(actions$Type=="ClickedToUnlock"),1]*sRate)
  msgballChosen_t <- ceiling(actions[which(actions$Type=="ballSelect"),1]*sRate)
  msgBallMoved_t <- ceiling(actions[which(actions$Type=="ballMove"),1]*sRate)
  msgBallClickedInBlockedMode_t <- ceiling(actions[which(actions$Type=="BallClickedInBlockedMode"),1]*sRate)
  msgBoardClickedInBlockedMode_t <- ceiling(actions[which(actions$Type=="BoardClickedInBlockedMode"),1]*sRate)
  
  epoch_size <- round(((epoch_size - left_border)/1000) * sRate)
  if(no_button_press) {
    if(ball_only){
      eventsT_t <- c(msgballChosen_t)
    } else {
      eventsT_t <- c(msgballChosen_t, msgBallMoved_t)
    }
  } else {
    eventsT_t <- c(msgbuttonPressed_t, msgballChosen_t, msgBallMoved_t)
  }
  
  
  if(ball_only){
    eventsNT_t <- c(msgBallClickedInBlockedMode_t)
  } else {
    eventsNT_t <- c(msgBallClickedInBlockedMode_t, msgBoardClickedInBlockedMode_t)
  }
  
  test_NT <- eventsNT_t
  
  if(!n_random_nontarget)
  {
    n_random_nontarget = length(eventsT_t)
  }
  
  if(random_non_target){
    eventsNT_t <- sort(
      ceiling(
        seq(
          from = ceiling(actions$Latency[1]*sRate) - left_border,
          to = nrow(signal) - epoch_size,
          length.out = n_random_nontarget #length(eventsT_t) * more_non_target
        )
      )
    )
  }
  
  eventsNT_t <- eventsNT_t[eventsNT_t<=nrow(signal)]
  eventsT_t <- eventsT_t[eventsT_t<=nrow(signal)]
  if(!length(test_NT)) {
    test_NT <- eventsNT_t
  } else {
    test_NT <- test_NT[test_NT<=nrow(signal)]
  }
  
  
  #   cat("Train epochs: target=", length(eventsT_t), " nontarget=", length(eventsNT_t), "\n")
  
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
  
  eegNT_test = array(dim = c(epoch_size, nChannels-1, length(test_NT)))
  for (i in 1:length(test_NT))
  {
    eegNT_test[ , , i] <- signal[(test_NT[i]+left_border+1):(test_NT[i]+epoch_size+left_border), -ncol(signal)]
  }
  
  l <- list(eegT = eegT, eegNT = eegNT, eegNT_test = eegNT_test)
  
  return(l)
  
}