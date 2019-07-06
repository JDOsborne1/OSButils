# Utility Scripts

get_hist <- function(df, ...){
  require(pipeR)
  df %>>%
    ggplot(aes(x = ...)) +
    geom_bar()
}

devCom <- function(input, comment){
  running_mode <- get0("running_mode", envir = .GlobalEnv, ifnotfound = "Dev")
  if(running_mode == "Dev"){
    print(comment)
  }
  return(input)
  
}

step <- 1
printAndIncrementStep <- function(value){
  assign(deparse(substitute(value)), value + 1, envir = .GlobalEnv)
  return(value)
}

printStepTitle <- function(string){
  print(paste0("Step ", printAndIncrementStep(step), ": ", string, " - ", strftime(Sys.time(),"%H:%M")))
}


getPropVect <- function(vect){
  return(vect/sum(vect))
}
