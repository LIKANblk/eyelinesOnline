draw_eeg_epochs <- function(summary_table, summary_eeg) {
  field_epochs <- summary_eeg[which(summary_table$field_type == 'field')]
  ball_epochs <- summary_eeg[which(summary_table$field_type == 'ball')]
  
  smaller_epoch_field <- c()
  for(i in 1:length(field_epochs)) {
    smaller_epoch_field[i] <- dim(field_epochs[[i]])[1]
  }
  smaller_epoch_field <- min(smaller_epoch_field)
  
  smaller_epoch_balls <- c()
  for(i in 1:length(ball_epochs)) {
    smaller_epoch_balls[i] <- dim(ball_epochs[[i]])[1]
  }
  smaller_epoch_balls <- min(smaller_epoch_balls)
  
  field_epochs <- lapply(field_epochs, function(x) { x[1:smaller_epoch_field, ]})
  ball_epochs <- lapply(ball_epochs, function(x) { x[1:smaller_epoch_balls, ]})
  
  all_ball_epochs <- array(unlist(ball_epochs), dim = c(nrow(ball_epochs[[1]]), ncol(ball_epochs[[1]]), length(ball_epochs)))
  all_field_epochs <- array(unlist(field_epochs), dim = c(nrow(field_epochs[[1]]), ncol(field_epochs[[1]]), length(field_epochs)))
  
  mean_ball_epochs <- apply(all_ball_epochs, c(3,2), mean)
  mean_field_epochs <- apply(all_field_epochs, c(3,2), mean)
  
  df_for_plot <- melt(mean_ball_epochs)
}