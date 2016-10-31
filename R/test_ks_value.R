test_ks_value <- function(experiment){
  
  data <- c()
  passed <- 0
  total <- 0
  
  lapply(experiment, function(exp){
    if(exp$file_data$record_type == 'test'){
      data <<- c(data, diff(which(exp$eeg_data$classifierOut$passed))-1)
      passed <<- passed + sum(exp$eeg_data$classifierOut$passed)
      total <<- total + nrow(exp$eeg_data$classifierOut)
    }
  })

  y = rnbinom(length(data), 1, passed/total)
  
  # w.dots <- data
  # c.dots <- y
  # 
  # library(boot)
  # library(MASS)
  # n.boot <- 1000
  # w.fit.boot <- boot(w.dots,R=n.boot,
  #                    statistic=function(xx,index)fitdistr(xx[index],densfun="negative binomial")$estimate)
  # c.fit.boot <- boot(c.dots,R=n.boot,
  #                    statistic=function(xx,index)fitdistr(xx[index],densfun="negative binomial")$estimate)
  # 
  # plot(c.fit.boot$t,pch=21,bg="black",xlab="size",ylab="mu",log="xy",cex=0.5,
  #      xlim=range(rbind(w.fit.boot$t,c.fit.boot$t)[,1]),
  #      ylim=range(rbind(w.fit.boot$t,c.fit.boot$t)[,2]))
  # abline(v=c.fit.boot$t0[1]); abline(h=c.fit.boot$t0[2])
  # points(w.fit.boot$t,pch=21,bg="red",col="red",cex=0.5)
  # abline(v=w.fit.boot$t0[1],col="red"); abline(h=w.fit.boot$t0[2],col="red")
  # legend(x="bottomright",inset=.01,pch=21,col=c("black","red"),pt.bg=c("black","red"),
  #        legend=c("c.dots","w.dots"))
  
  KolmogorovSmirnov(data, y)
}