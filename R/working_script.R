draw_nasa <- function() {
  
  path <- '/home/mayenok/Yandex.Disk/eyelinesOnlineNew/data/'
  
  nasa_all <- list()
  ii <- 1
  for (i in 23:27) {
    nasa_all[[ii]] <- process_nasa(paste0(path, i, '/'))
    ii <- ii + 1
  }
  
  df <- do.call( rbind,
                 lapply(nasa_all, function(x) 
                   data.frame(
                     type=c('Prepare_Clf', 'Normal_Clf', 'Random_Clf'),
                     joy_level = c(x$clf_prep$joy_level, x$normal_clf$joy_level, x$random_clf$joy_level),
                     workload = c(x$clf_prep$results_overall, x$normal_clf$results_overall, x$random_clf$results_overall))
                 )
  )
  
  df$type <- factor(df$type, levels = c('Prepare_Clf', 'Normal_Clf', 'Random_Clf'), ordered = T)
  
  joy_means <- aggregate(joy_level ~  type, df, mean)
  workload_means <- aggregate(workload ~  type, df, mean)
  
  
  # ggplot(data=df, aes(type, joy_level))+geom_boxplot()
  p <- ggplot(df, aes(type, joy_level, fill = type)) +
    geom_boxplot() +
    scale_fill_brewer(palette = "Greens") +
    labs(x="", y="Уровень удовольствия\n") +
    theme(legend.position="none") +
    scale_x_discrete(labels = c("Prepare_Clf" = "Подготовка\nклассификатора",
                                "Normal_Clf" = "Нормальный\nклассификатор",
                                "Random_Clf" = "Случайный\nклассификатор")) +
    geom_text(data = joy_means, aes(label = joy_level, y = 90), colour = 'red4', size=5)
  
  p 
  
  
  p2 <- ggplot(df, aes(type, workload, fill = type)) +
    geom_boxplot() +
    scale_fill_brewer(palette = "Blues") +
    labs(x="", y="Уровень нагрузки\n") +
    theme(legend.position="none") +
    scale_x_discrete(labels = c("Prepare_Clf" = "Подготовка\nклассификатора",
                                "Normal_Clf" = "Нормальный\nклассификатор",
                                "Random_Clf" = "Случайный\nклассификатор")) +
    geom_text(data = workload_means, aes(label = workload, y = 90), colour = 'red4', size=5)
  
  p2
  
  df_weighted_rating <- do.call( rbind,
                                 lapply(nasa_all, function(x) 
                                   data.frame(
                                     type=c('Prepare_Clf', 'Normal_Clf', 'Random_Clf'),
                                     mental_demand = c(x$clf_prep$results_rating[1] * x$clf_prep$results_weight[1],
                                                       x$normal_clf$results_rating[1] * x$normal_clf$results_weight[1],
                                                       x$random_clf$results_rating[1] * x$random_clf$results_weight[1]),
                                     physical_demand = c(x$clf_prep$results_rating[2] * x$clf_prep$results_weight[2],
                                                         x$normal_clf$results_rating[2] * x$normal_clf$results_weight[2],
                                                         x$random_clf$results_rating[2] * x$random_clf$results_weight[2]),
                                     temporal_demand = c(x$clf_prep$results_rating[3] * x$clf_prep$results_weight[3],
                                                         x$normal_clf$results_rating[3] * x$normal_clf$results_weight[3],
                                                         x$random_clf$results_rating[3] * x$random_clf$results_weight[3]),
                                     overall_performance = c(x$clf_prep$results_rating[4] * x$clf_prep$results_weight[4],
                                                             x$normal_clf$results_rating[4] * x$normal_clf$results_weight[4],
                                                             x$random_clf$results_rating[4] * x$random_clf$results_weight[4]),
                                     frustration_level = c(x$clf_prep$results_rating[5] * x$clf_prep$results_weight[5],
                                                           x$normal_clf$results_rating[5] * x$normal_clf$results_weight[5],
                                                           x$random_clf$results_rating[5] * x$random_clf$results_weight[5]),
                                     effort = c(x$clf_prep$results_rating[6] * x$clf_prep$results_weight[6],
                                                x$normal_clf$results_rating[6] * x$normal_clf$results_weight[6],
                                                x$random_clf$results_rating[6] * x$random_clf$results_weight[6]))
                                 )
  )
  df_weighted_rating$type <- factor(df_weighted_rating$type, levels = c('Prepare_Clf', 'Normal_Clf', 'Random_Clf'), ordered = T)
  mental_demand_means <- aggregate(mental_demand ~  type, df_weighted_rating, mean)
  physical_demand_means <- aggregate(physical_demand ~  type, df_weighted_rating, mean)
  temporal_demand_means <- aggregate(temporal_demand ~  type, df_weighted_rating, mean)
  overall_performance_means <- aggregate(overall_performance ~  type, df_weighted_rating, mean)
  frustration_level_means <- aggregate(frustration_level ~  type, df_weighted_rating, mean)
  effort_means <- aggregate(effort ~  type, df_weighted_rating, mean)
  
  clf_df_test <- data.frame()
  clf_df_random <- data.frame()
  for(i in 23:27) {
    res <- prepare_results_clf(paste0(path, i,'/experiment.RData'))
    clf_df_test <- rbind(clf_df_test, res$normal)
    clf_df_random <- rbind(clf_df_random, res$random)
  }
  clf_df_test$type <- factor(clf_df_test$type, levels = c('TP', 'TN', 'FP', 'FN'), ordered = T)
  clf_df_random$type <- factor(clf_df_random$type, levels = c('TP', 'TN', 'FP', 'FN'), ordered = T)
  
  clf_df_test_means <- aggregate(clf_count ~  type, clf_df_test, mean)
  clf_df_random_means <- aggregate(clf_count ~  type, clf_df_random, mean)
  
  
  
  p1 <- ggplot(df_weighted_rating, aes(type, effort, fill = type)) +
    geom_boxplot() +
    scale_fill_brewer(palette = "Blues") +
    labs(x="", y="Уровень нагрузки\n") +
    theme(legend.position="none") +
    labs(title = "Затраченные усилия") +
    scale_x_discrete(labels = c("Prepare_Clf" = "Подготовка\nклассификатора",
                                "Normal_Clf" = "Нормальный\nклассификатор",
                                "Random_Clf" = "Случайный\nклассификатор")) +
    geom_text(data = effort_means, aes(label = effort, y = 30), colour = 'red4', size=5)
  
  
  p_clf <- ggplot(clf_df_random, aes(type, clf_count, fill = type)) +
    geom_boxplot() +
    scale_fill_brewer(palette = "Reds") +
    labs(x="", y="Количество ответов\n") +
    theme(legend.position="none") +
    labs(title = "Случайный классификатор") +
    scale_x_discrete(labels = c("TP" = "True\npositive",
                                "TN" = "True\nnegative",
                                "FP" = "False\npositive",
                                "FN" = "False\nnegative")) +
    geom_text(data = clf_df_random_means, aes(label = clf_count, y = 170), colour = 'red4', size=5)
  
  p_clf
  
}

create_dfs_for_wolcoxon <- function() {
  random_changed <- c()
  normal_changed <- c()
  
  for (i in c(10:12, 14:27)) {
    load(paste0(path, i,'/experiment.RData'))
    for(i in 1:length(experiment)){
      if(experiment[[i]]$file_data$record_type == 'random') {
        random_changed <- c(random_changed, experiment[[i]]$file_data$changed_selection)
      } else if (experiment[[i]]$file_data$record_type == 'test') {
        normal_changed <- c(normal_changed, experiment[[i]]$file_data$changed_selection)
      }
    }
    remove(experiment)
  }
  
  random_FP <- c()
  normal_FP <- c()
  
  for (i in 23:27) {
    load(paste0(path, i,'/experiment.RData'))
    for(i in 1:length(experiment)){
      if(experiment[[i]]$file_data$record_type == 'random') {
        random_FP <- c(random_FP, sum(experiment[[i]]$events$false_alarm == T))
      } else if (experiment[[i]]$file_data$record_type == 'test') {
        normal_FP <- c(normal_FP, sum(experiment[[i]]$events$false_alarm == T))
      }
    }
    remove(experiment)
  }
}