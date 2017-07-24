
for (exp in c(29,30,32:38)){
  print(sprintf('EXPERIMENT NUMBER %d',exp))
  aucs <- c()
  sens <- c()
  spec <- c()
  for (try in 1:10){
    res = buildClassifier(sprintf("/home/likan_blk/Yandex.Disk/eyelinesOnlineNew/data/%d/",exp), no_button_press = T, random_non_target = T, ball_only = T, n_random_nontarget = FALSE, low=0.2, high=30)
    aucs[try] =res$quality$auc$tst[1]
    sens[try] =res$quality$sens$tst[1]
    spec[try] =res$quality$spec$tst[1]
    
  }
  print(sprintf('Target epochs %d',res$target_epochs))
  print(sprintf('NonTarget epochs  %d',res$nontarget_epochs))
  print(sprintf('AUC test %f±%f',mean(aucs), std(aucs)))
  print(sprintf('SENS test %f±%f',mean(sens), std(sens)))
  print(sprintf('SPEC test %f±%f',mean(spec), std(spec)))
}