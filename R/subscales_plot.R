subscales_plots <- function(){
  p1 <- ggplot(df_weighted_rating, aes(type, mental_demand, fill = type)) +
    geom_boxplot() +
    scale_fill_brewer(palette = "Blues") +
    labs(x="", y="") +
    theme(legend.position="none", axis.title.x=element_blank(),axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), plot.title = element_text(size = 9),
          plot.margin = unit(c(0.3,0.2,1,0), "cm")) +
    labs(title = "Умственная нагрузка") +
    geom_text(data = mental_demand_means, aes(label = mental_demand, y = 30), colour = 'red4', size=4)
  
  p2 <- ggplot(df_weighted_rating, aes(type, physical_demand, fill = type)) +
    geom_boxplot() +
    scale_fill_brewer(palette = "Blues") +
    theme(legend.position="none", axis.title.x=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(), axis.title.y=element_blank(),
          axis.ticks=element_blank(), plot.title = element_text(size = 9),
          plot.margin = unit(c(0.3,0.5,1,0.7), "cm")) +
    labs(title = "Физическая нагрузка") +
    geom_text(data = physical_demand_means, aes(label = physical_demand, y = 30), colour = 'red4', size=4)
  
  p3 <- ggplot(df_weighted_rating, aes(type, temporal_demand, fill = type)) +
    geom_boxplot() +
    scale_fill_brewer(palette = "Blues") +
    theme(legend.position="none", axis.title.x=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(), axis.title.y=element_blank(),
          axis.ticks=element_blank(), plot.title = element_text(size = 9),
          plot.margin = unit(c(0.3,0.5,1,0.7), "cm")) +
    labs(title = "Давление времени") +
    geom_text(data = temporal_demand_means, aes(label = temporal_demand, y = 30), colour = 'red4', size=4)
  
  p4 <- ggplot(df_weighted_rating, aes(type, overall_performance, fill = type)) +
    geom_boxplot() +
    scale_fill_brewer(palette = "Blues") +
    labs(x="", y="") +
    theme(legend.position="none", plot.title = element_text(size = 9),
          plot.margin = unit(c(0.2,0.2,0.2,0), "cm")) +
    labs(title = "Успешность выполнения") +
    scale_x_discrete(labels = c("Prepare_Clf" = "Подготовка\nклф.",
                                "Normal_Clf" = "Нормальный\nклф.",
                                "Random_Clf" = "Случайный\nклф.")) +
    geom_text(data = overall_performance_means, aes(label = overall_performance, y = 30), colour = 'red4', size=4)
  
  p5 <- ggplot(df_weighted_rating, aes(type, frustration_level, fill = type)) +
    geom_boxplot() +
    scale_fill_brewer(palette = "Blues") +
    labs(x="", y="") +
    theme(legend.position="none", axis.ticks=element_blank(),
          axis.text.y=element_blank(), axis.title.y=element_blank(),
          plot.title = element_text(size = 9),
          plot.margin = unit(c(0.2,0.5,0,0.7), "cm")) +
    labs(title = "Фрустрация") +
    scale_x_discrete(labels = c("Prepare_Clf" = "Подготовка\nклф.",
                                "Normal_Clf" = "Нормальный\nклф.",
                                "Random_Clf" = "Случайный\nклф.")) +
    geom_text(data = frustration_level_means, aes(label = frustration_level, y = 30), colour = 'red4', size=4)
  
  p6 <- ggplot(df_weighted_rating, aes(type, effort, fill = type)) +
    geom_boxplot() +
    scale_fill_brewer(palette = "Blues") +
    labs(x="", y="") +
    theme(legend.position="none", axis.ticks=element_blank(),
          axis.text.y=element_blank(), axis.title.y=element_blank(),
          plot.title = element_text(size = 9),
          plot.margin = unit(c(0.2,0.5,0,0.7), "cm")) +
    labs(title = "Затраченные усилия") +
    scale_x_discrete(labels = c("Prepare_Clf" = "Подготовка\nклф.",
                                "Normal_Clf" = "Нормальный\nклф.",
                                "Random_Clf" = "Случайный\nклф.")) +
    geom_text(data = effort_means, aes(label = effort, y = 30), colour = 'red4', size=4)
  
  
  grid.arrange(p1, p2, p3, p4, p5, p6, 
               left = textGrob("Уровень нагрузки", gp=gpar(fontsize = 12), rot = 90),
               layout_matrix = matrix(c(1,2,3,4,5,6), ncol=3, byrow=TRUE))
  
}