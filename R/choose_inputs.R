choose_inputs <- function(z,single_inputs) {
  chosen_inputs <- list()
  q <- 1
  if(length(single_inputs>0)){
    for(i in 1:length(single_inputs)){
      for (ii in 1:length(z)){
        if(single_inputs[i] %in% z[[ii]]){
          chosen_inputs[[q]] <- z[[ii]]
          q <- q + 1
        }
      }
    }
    chosen_inputs
  }
}