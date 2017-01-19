# for (i in 23:27){
#   res <- buildClassifier( paste0("/home/mayenok/Yandex.Disk/eyelinesOnlineNew/data/", i,"/"), no_button_press = T, random_non_target = F, ball_only = T)
#   cat(i, ' - ', format(res$quality$auc$tst[1], digits = 2), '+-', format(res$quality$auc$tst[2], digits = 2), '\n')
# }

show_all_clf_results <- function(){
  for (i in 23:27){
    res <- buildClassifier( paste0("/home/mayenok/Yandex.Disk/eyelinesOnlineNew/data/", i,"/"),
                            no_button_press = T, random_non_target = T)
    cat('Experiment ', i, '\n\n', sprintf('
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
  }
}


