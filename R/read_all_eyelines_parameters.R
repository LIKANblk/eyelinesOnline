read_all_eyelines_parameters <- function(messages){
  do.call(c, lapply( str_filter(messages, '^settings (\\{.+\\})'), function(str){
    jsonlite::fromJSON(str[[2]])
  }))
}