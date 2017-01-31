randov_vs_normal_clf_compare_tables <- function(){
  df_random_ball <- data.frame()
  df_test_ball <- data.frame()
  df_random_field <- data.frame()
  df_test_field <- data.frame()
  
  for( i in c(29, 30, 32, 33)) {
    df_random_ball <- rbind(df_random_ball, task_processing(paste0('~/Yandex.Disk/eyelinesOnlineNew/data/', i, '/experiment.RData'), 'random', 'ball'))
    df_test_ball <- rbind(df_test_ball,  task_processing(paste0('~/Yandex.Disk/eyelinesOnlineNew/data/', i, '/experiment.RData'), 'test', 'ball'))
  }
  
  for( i in c(29, 30, 32, 33)) {
    df_random_field <- rbind(df_random_field, task_processing(paste0('~/Yandex.Disk/eyelinesOnlineNew/data/', i, '/experiment.RData'), 'random', 'field'))
    df_test_field <- rbind(df_test_field,  task_processing(paste0('~/Yandex.Disk/eyelinesOnlineNew/data/', i, '/experiment.RData'), 'test', 'field'))
  }
  
  write.table(df_random_ball, '/home/mayenok/Yandex.Disk/eyelinesOnlineNew/df_random_ball.csv', quote = F, row.names = F)
  write.table(df_test_ball, '/home/mayenok/Yandex.Disk/eyelinesOnlineNew/df_test_ball.csv', quote = F, row.names = F)
  write.table(df_random_field, '/home/mayenok/Yandex.Disk/eyelinesOnlineNew/df_random_field.csv', quote = F, row.names = F)
  write.table(df_test_field, '/home/mayenok/Yandex.Disk/eyelinesOnlineNew/df_test_field.csv', quote = F, row.names = F)
  
}


# df_random_ball <- data.frame()
# df_test_ball <- data.frame()
# df_random_field <- data.frame()
# df_test_field <- data.frame()
# 
# df_random_ball <- rbind(df_random_ball, task_processing(paste0('~/Yandex.Disk/eyelinesOnlineNew/data/', 30, '/experiment.RData'), 'random', 'ball'))
# df_test_ball <- rbind(df_test_ball,  task_processing(paste0('~/Yandex.Disk/eyelinesOnlineNew/data/', 30, '/experiment.RData'), 'test', 'ball'))
# 
# df_random_field <- rbind(df_random_field, task_processing(paste0('~/Yandex.Disk/eyelinesOnlineNew/data/', 30, '/experiment.RData'), 'random', 'field'))
# df_test_field <- rbind(df_test_field,  task_processing(paste0('~/Yandex.Disk/eyelinesOnlineNew/data/', 30, '/experiment.RData'), 'test', 'field'))
# 
# write.table(df_random_ball, '/home/mayenok/Yandex.Disk/eyelinesOnlineNew/df_random_ball_30.csv', quote = F, row.names = F)
# write.table(df_test_ball, '/home/mayenok/Yandex.Disk/eyelinesOnlineNew/df_test_ball_30.csv', quote = F, row.names = F)
# write.table(df_random_field, '/home/mayenok/Yandex.Disk/eyelinesOnlineNew/df_random_field_30.csv', quote = F, row.names = F)
# write.table(df_test_field, '/home/mayenok/Yandex.Disk/eyelinesOnlineNew/df_test_field_30.csv', quote = F, row.names = F)
