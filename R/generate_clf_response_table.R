generate_clf_response_table <- function(path, ball =  T) {
  df <- data.frame()
  for(i in 23:27) {
    res <- prepare_results_clf(paste0(path, i,'/experiment.RData'), ball)
    tmp <- data.frame(TP_normal = res$normal$clf_count[which(res$normal$type == 'TP')],
                      TN_normal = res$normal$clf_count[which(res$normal$type == 'TN')],
                      FP_normal = res$normal$clf_count[which(res$normal$type == 'FP')],
                      FN_normal = res$normal$clf_count[which(res$normal$type == 'FN')],
                      TP_random = res$random$clf_count[which(res$random$type == 'TP')],
                      TN_random = res$random$clf_count[which(res$random$type == 'TN')],
                      FP_random = res$random$clf_count[which(res$random$type == 'FP')],
                      FN_random = res$random$clf_count[which(res$random$type == 'FN')])
    df <- rbind(df, tmp)
  }
  mean_df <- c(mean(df$TP_normal), mean(df$TN_normal), mean(df$FP_normal), mean(df$FN_normal),
               mean(df$TP_random), mean(df$TN_random), mean(df$FP_random), mean(df$FN_random))
  std_df <- c(std(df$TP_normal), std(df$TN_normal), std(df$FP_normal), std(df$FN_normal),
              std(df$TP_random), std(df$TN_random), std(df$FP_random), std(df$FN_random))
  
  list_percents <- as.list(as.data.frame(t(df)))
  normal_sums <- sapply(list_percents, function(x) {sum(unlist(x)[1:4]) })
  random_sums <- sapply(list_percents, function(x) {sum(unlist(x)[5:8]) })
  # 
  # df_percents <- data.frame(df[, 1:4]*100/normal_sums)
  
  #df <- rbind(df, rbind(mean_df, std_df))
  df
}

# df_ball <- generate_clf_response_table(path)
# df_cell <- generate_clf_response_table(path, ball = F)
# write.table(df_ball, file="/home/mayenok/Yandex.Disk/eyelinesOnlineNew/ball.csv", row.names = F, col.names = F)
# write.table(df_cell, file="/home/mayenok/Yandex.Disk/eyelinesOnlineNew/cell.csv", row.names = F, col.names = F)