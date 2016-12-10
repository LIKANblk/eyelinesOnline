process_nasa <- function(path) {
  clf_prep <- eval(parse(text=gsub('nasa_tlx_results = ', '', paste(readLines(paste0(path,'nasa_prepare_clf.txt'), warn = F), collapse=""))))
  normal_clf <- eval(parse(text=gsub('nasa_tlx_results = ', '', paste(readLines(paste0(path,'nasa_clf.txt'), warn = F), collapse=""))))
  random_clf <- eval(parse(text=gsub('nasa_tlx_results = ', '', paste(readLines(paste0(path,'nasa_random.txt'), warn = F), collapse=""))))
  l <- list(clf_prep = clf_prep,
            normal_clf = normal_clf,
            random_clf = random_clf)
  l
}