# for (i in 23:27){
#   res <- buildClassifier( paste0("/home/mayenok/Yandex.Disk/eyelinesOnlineNew/data/", i,"/"), no_button_press = T, random_non_target = F, ball_only = T)
#   cat(i, ' - ', format(res$quality$auc$tst[1], digits = 2), '+-', format(res$quality$auc$tst[2], digits = 2), '\n')
# }

show_all_clf_results <- function(n_random_nontarget){
  auc_tests <- c()
  auc_tests_est <- c()
  
  for (i in 23:27){
    res <- buildClassifier( paste0("/home/mayenok/Yandex.Disk/eyelinesOnlineNew/data/", i,"/"),
                            no_button_press = F, random_non_target = T, ball_only = F,
                            n_random_nontarget = n_random_nontarget)
    cat(
'Experiment ', i, '\n\n', sprintf('
Target epochs:   %i
Nontarget epochs: %i

Specifity:
 Train: %.3f ± %.3f
 Test:  %.3f ± %.3f
Sensivity:
 Train: %.3f ± %.3f
 Test:  %.3f ± %.3f
Accuracy:
 Train: %.3f ± %.3f
 Test:  %.3f ± %.3f
AUC:
 Train: %.3f ± %.3f
 Test:  %.3f ± %.3f
 All:   %.3f ± %.3f
------------------------------------------------

',
        res$target_epochs, res$nontarget_epochs,
        res$quality$spec$tr[1],  res$quality$spec$tr[2],
        res$quality$spec$tst[1], res$quality$spec$tst[2],
        
        res$quality$sens$tr[1],  res$quality$sens$tr[2],
        res$quality$sens$tst[1], res$quality$sens$tst[2],
        
        res$quality$acc$tr[1],  res$quality$acc$tr[2],
        res$quality$acc$tst[1], res$quality$acc$tst[2],
        
        res$quality$auc$tr[1],  res$quality$auc$tr[2],
        res$quality$auc$tst[1], res$quality$auc$tst[2],
        res$quality$auc$all[1], res$quality$auc$all[2]))
  
    auc_tests <- c(auc_tests, res$quality$auc$tst[1])
    auc_tests_est <- c(auc_tests_est, res$quality$auc$tst[2])
    
  }
  
  cat('\nAuc test all ')
  n_exp <- 23
  for(i in 1:length(auc_tests)){
    cat('Experiment ', n_exp, ': ', auc_tests[i], ' +- ', auc_tests_est[i], '\n')
    n_exp <- n_exp + 1
  }
}


