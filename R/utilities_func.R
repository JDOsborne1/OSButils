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

#' One Stop Data Type Formatting
#'
#' A function to perform the routine data type transformations at the beggining of a data flow in one easy go.
#' Most data is read into R as character data, but most useful data we work with is not character data,
#' this package allows you to direct R to transform all the columns suiting a regular expression you define to
#' be transformed as either date, numeric or factor data.
#'
#' @param dfin The input dataframe
#' @param dateform The format of any dates in the dataframe as a string
#' @param datereg The regular expression to determine if a column name justifies treating it as a date
#' @param numreg The regular expression to determine if a column name justifies treating it as numeric
#' @param flagreg The regular expression to determine if a column name justifies treating it as a factor
#'
#' @return The dataframe with the appropriate data type transformations applied to the columns
#' @export
#'
#' @examples
data_type_format <- function(dfin, dateform = "%Y-%m-%d", datereg = "date", numreg = "value", flagreg = "flag") {
  dfout <- dfin %>%
    mutate_at(vars(matches(datereg)),funs(as.Date(as.character(.),format=dateform)))%>%
    mutate_at(vars(matches(numreg)),funs(as.numeric))%>%
    mutate_at(vars(matches(flagreg)),funs(as.factor))
  return(dfout)
}
